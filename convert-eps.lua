-- Known incompatibilities:
--  - Many missing commands
--  - Substrings and array intervals don't share their value with the full string/array
--  - Access properties are not enforced

local pdfprint -- Set later to have the right mode

local l = lpeg

local whitespace = (l.S'\0\t\n\r\f ' + '%' * (1-l.P'\n')^0 * (l.P'\n' + -1))^1

local regular = 1 - l.S'\0\t\n\r\f %()<>[]{}/'

-- local integer = l.S'+-'^-1 * l.R'09'^1 / tonumber
local real = l.S'+-'^-1 * (l.R'09'^1 * ('.' * l.R'09'^0)^-1 + '.' * l.R'09'^1) * (l.S'Ee' * l.S'+-'^-1 * l.R'09'^1)^-1 / tonumber
local radix_scanner = setmetatable({}, {__index = function(t, b)
  local digit
  if b < 10 then
    digit = l.R('0' .. string.char(string.byte'0' + b - 1))
  else
    digit = l.R'09'
    if b > 10 then
      digit = digit + l.R('A' .. string.char(string.byte'A' + b - 11))
      digit = digit + l.R('a' .. string.char(string.byte'a' + b - 11))
    end
  end
  digit = l.C(digit^1) * l.Cp()
  t[b] = digit
  return digit
end})
local radix = l.Cmt(l.R'09' * l.R'09'^-1 / tonumber * '#', function(subj, pos, radix)
  if radix < 2 or radix > 36 then return end
  local digits, pos = radix_scanner[radix]:match(subj, pos)
  if not digits then return end
  digits = tonumber(digits, radix)
  return pos, digits
end)
local number = radix + real -- + integer -- Every integer is also a real

local string_patt do
  local literal = '(' * l.Cs(l.P{(
      l.Cg('\\' * (
          'n' * l.Cc'\n'
        + 'r' * l.Cc'\r'
        + 't' * l.Cc'\t'
        + 'b' * l.Cc'\b'
        + 'f' * l.Cc'\f'
        + '\\' * l.Cc'\\'
        + '(' * l.Cc'('
        + ')' * l.Cc')'
        + l.R'07' * l.R'07'^-2 / function(s) return string.char(tostring(s, 8) % 0x100) end
        + ('\r' * l.P'\n'^-1 + '\n') * l.Cc''
      ))
    + l.Cg('\r' * l.P'\n'^-1 * l.Cc'\n')
    + (1-l.S'()')
    + '(' * l.V(1) * ')'
  )^0}) * ')'
  local hexchar = l.R('09', 'af', 'AF')
  local hexbyte = hexchar * hexchar^-1 / function(s)
    local b = tonumber(s, 16)
    return #s == 1 and 16*b or b
  end
  local hex = '<' * (hexbyte^0 / string.char) * '>'
  string_patt = literal + hex -- TODO: Base85 is not implemented
end

local name = l.C(regular^1 + l.S'[]' + '<<' + '>>')
local literal_name = '/' * l.C(regular^0)
local imm_name = '//' * l.C(regular^0)

-- All objects are literal by default, except names represented as direct strings and operators
local any_object = l.P{whitespace^-1 * (
    number * -regular
  + l.Ct(l.Cg(string_patt, 'value') * l.Cg(l.Cc'string', 'kind'))
  + name
  + l.Ct(l.Cg(literal_name, 'value') * l.Cg(l.Cc'name', 'kind'))
  -- + imm_name * l.Cc'immediate_name' -- TODO???
  + l.Ct(l.Cg(l.Ct(l.Cg('{' * l.Ct(l.V(1)^0) * whitespace^-1 * '}', 'value') * l.Cg(l.Cc'array', 'kind')), 'value') * l.Cg(l.Cc'executable', 'kind'))
)}
local object_list = l.Ct(any_object^0) * whitespace^-1 * (-1 + l.Cp())

local function parse_ps(s)
  local tokens, fail_offset = object_list:match(s)
  if fail_offset then
    error(string.format('Failed to parse PS tokens at `%s\'', s:sub(fail_offset)))
  end
  return tokens
end

local operand_stack = {}

local function push(value)
  operand_stack[#operand_stack+1] = value
end
local function pop()
  local height = #operand_stack
  if height == 0 then
    error'Popping from empty stack'
  end
  local v = operand_stack[height]
  operand_stack[height] = nil
  return v
end
local pop_num = pop
local pop_int = pop
local function pop_proc()
  local v = pop()
  if type(v) ~= 'table' or v.kind ~= 'executable' or type(v.value) ~= 'table' or v.value.kind ~= 'array' then
    error'typecheck'
  end
  return v.value.value
end
local pop_bool = pop
local pop_dict = pop
local pop_array = pop
local pop_string = pop
local function pop_key()
  local key = pop()
  if type(key) == 'table' then
    local kind = key.kind
    if kind == 'executable' then
      key = key.value
      if type(key) == 'table' and key.kind == 'string' then
        key = key.value
      end
    elseif kind == 'executable' or kind == 'name' then
      key = key.value
    end
  end
  return key
end

local execute_ps, execute_tok

local dictionary_stack
local graphics_stack = {{
  matrix = {1, 0, 0, 1, 0, 0},
  linewidth = nil,
  current_path = nil,
  current_point = nil,
  color = {},
  fillconstantalpha = 1,
  strokeconstantalpha = 1,
  alphaisshape = false,
  linejoin = nil,
  linecap = nil,
  strokeadjust = nil,
}}

local function matrix_transform(x, y, xx, xy, yx, yy, dx, dy)
  return x * xx + y * yx + dx, x * xy + y * yy + dy
end
local function matrix_invert(xx, xy, yx, yy, dx, dy)
  local determinante = xx*yy - xy*yx
  xx, xy, yx, yy = yy/determinante, -xy/determinante, -yx/determinante, xx/determinante
  dx, dy = - dx * xx - dy * yx, - dx * xy - dy * yy
  return xx, xy, yx, yy, dx, dy
end
local function update_matrix(xx, xy, yx, yy, dx, dy)
  pdfprint(string.format('%.3f %.3f %.3f %.3f %.3f %.3f cm', xx, xy, yx, yy, dx, dy))
  local matrix = graphics_stack[#graphics_stack].matrix
  matrix[1], matrix[2],
  matrix[3], matrix[4],
  matrix[5], matrix[6]
    = xx * matrix[1] + xy * matrix[3],             xx * matrix[2] + xy * matrix[4],
      yx * matrix[1] + yy * matrix[3],             yx * matrix[2] + yy * matrix[4],
      dx * matrix[1] + dy * matrix[3] + matrix[5], dx * matrix[2] + dy * matrix[4] + matrix[6]

  local current_path = graphics_stack[#graphics_stack].current_path
  if not current_path then return end

  local determinante = xx*yy - xy*yx
  xx, xy, yx, yy, dx, dy = matrix_invert(xx, xy, yx, yy, dx, dy)
  local i=1
  while current_path[i] do
    local entry = current_path[i]
    if type(entry) == 'number' then
      local after = current_path[i+1]
      assert(type(after) == 'number')
      current_path[i], current_path[i+1] = xx * entry + yx * after + dx, xy * entry + yy * after + dy
      i = i+2
    else
      i = i+1
    end
  end
  local current_point = graphics_stack[#graphics_stack].current_point
  local x, y = current_point[1], current_point[2]
  current_point[1], current_point[2] = xx * x + yx * y + dx, xy * x + yy * y + dy
end

local function try_lookup(name)
  for i = #dictionary_stack, 1, -1 do
    local dict = dictionary_stack[i]
    local value = dict.value[name]
    if value ~= nil then
      return value
    end
  end
end
local function lookup(name)
  local result = try_lookup(name)
  if result == nil then
    return error(string.format('Unknown name %q', name))
  end
  return result
end
local function bind(proc)
  for i=1, #proc do
    local entry = proc[i]
    local tentry = type(entry)
    if tentry == 'table' and entry.kind == 'executable' and type(entry.value) == 'table' and entry.value.kind == 'array' then
      bind(entry.value.value)
    elseif tentry == 'string' then
      local res = try_lookup(entry)
      if type(res) == 'function' then
        proc[i] = res
      end
    end
  end
end

local mark = {kind = 'mark'}
local null = {kind = 'null'}
local globaldict = {kind = 'dict', value = {}}
local userdict = {kind = 'dict', value = {
  SDict = {kind = 'dict', value = {
    normalscale = {kind = 'executable', value = {kind = 'array', value = {}}},
  }},
}}
local systemdict systemdict = {kind = 'dict', value = {
  dup = function()
    local v = pop()
    push(v)
    push(v)
  end,
  exch = function()
    local b, a = pop() a = pop()
    push(b)
    push(a)
  end,
  pop = function()
    pop()
  end,
  copy = function()
    local n = pop_int()
    local height = #operand_stack
    if n > height then
      error'copy argument larger then stack'
    end
    table.move(operand_stack, height-n+1, height, height+1)
  end,
  roll = function()
    local j = pop_int()
    local n = pop_int()
    if n < 0 then error'Invalid roll size' end
    if n == 0 or j == 0 then return end
    local height = #operand_stack
    if j < 0 then
      j = (-j) % n
      local temp = table.move(operand_stack, height-n+1, height-n+j, 1, {})
      table.move(operand_stack, height-n+j+1, height, height-n+1)
      table.move(temp, 1, j, height-j+1, operand_stack)
    else
      j = j % n
      local temp = table.move(operand_stack, height-j+1, height, 1, {})
      table.move(operand_stack, height-n+1, height-j, height-n+j+1)
      table.move(temp, 1, j, height-n+1, operand_stack)
    end
  end,
  index = function()
    local i = pop_int()
    local height = #operand_stack
    if i < 0 or height <= i then error'Invalid index' end
    push(operand_stack[height - i])
  end,
  mark = function()
    push(mark)
  end,
  ['['] = function()
    push(mark)
  end,
  [']'] = function()
    systemdict.value.counttomark()
    systemdict.value.array()
    systemdict.value.astore()
    systemdict.value.exch()
    systemdict.value.pop()
  end,
  ['<<'] = function()
    push(mark)
  end,
  count = function()
    push(#operand_stack)
  end,
  counttomark = function()
    local height = #operand_stack
    for i=height, 1, -1 do
      local entry = operand_stack[i]
      if type(entry) == 'table' and entry.kind == 'mark' then
        return push(height-i)
      end
    end
    error'Unmatched mark'
  end,
  cleartomark = function()
    local entry
    repeat
      entry = pop()
    until (not entry) or type(entry) == 'table' and entry.kind == 'mark'
    if not entry then error'Unmatched mark' end
  end,

  ['if'] = function()
    local proc = pop_proc()
    local cond = pop_bool()
    if cond then
      execute_ps(proc)
    end
  end,
  ifelse = function()
    local proc2 = pop_proc()
    local proc1 = pop_proc()
    local cond = pop_bool()
    if cond then
      execute_ps(proc1)
    else
      execute_ps(proc2)
    end
  end,
  ['for'] = function()
    local proc = pop_proc()
    local limit = pop_num()
    local step = pop_num()
    local initial = pop_num()
    local coro = coroutine.wrap(function()
      for i=initial, limit, step do
        push(i)
        execute_ps(proc)
      end
    end)
    local result = coro(proc)
    if result == 'exit' or not result then
    else
      coroutine.yield(result)
    end
  end,
  ['repeat'] = function()
    local proc = pop_proc()
    local count = pop_num()
    local coro = coroutine.wrap(function()
      for i=1, count do
        execute_ps(proc)
      end
    end)
    local result = coro(proc)
    if result == 'exit' or not result then
    else
      coroutine.yield(result)
    end
  end,

  ['not'] = function()
    local val = pop()
    local tval = type(val)
    if tval == 'table' and val.kind == 'executable' then
      val = val.value
      local tval = type(val)
    end
    if tval == 'boolean' then
      push(not val)
    elseif tval == 'number' then
      push(~val)
    else
      error'typecheck'
    end
  end,
  ['and'] = function()
    local val = pop()
    local tval = type(val)
    if tval == 'table' and val.kind == 'executable' then
      val = val.value
      local tval = type(val)
    end
    if tval == 'boolean' then
      push(pop_bool() and val)
    elseif tval == 'number' then
      push(val & pop_int())
    else
      error'typecheck'
    end
  end,
  ['or'] = function()
    local val = pop()
    local tval = type(val)
    if tval == 'table' and val.kind == 'executable' then
      val = val.value
      local tval = type(val)
    end
    if tval == 'boolean' then
      push(pop_bool() and val)
    elseif tval == 'number' then
      push(val | pop_int())
    else
      error'typecheck'
    end
  end,
  ['xor'] = function()
    local val = pop()
    local tval = type(val)
    if tval == 'table' and val.kind == 'executable' then
      val = val.value
      local tval = type(val)
    end
    if tval == 'boolean' then
      push(val ~= pop_bool())
    elseif tval == 'number' then
      push(val ~ pop_int())
    else
      error'typecheck'
    end
  end,

  eq = function()
    local b, a = pop() a = pop()
    if type(a) == 'table' and (a.kind == 'executable' or a.kind == 'name' or a.kind == 'operator') then
      a = a.value
    end
    if type(b) == 'table' and (b.kind == 'executable' or b.kind == 'name' or b.kind == 'operator') then
      b = b.value
    end
    push(a==b)
  end,
  gt = function()
    local b, a = pop() a = pop()
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        error'typecheck'
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        error'typecheck'
      end
      a, b = a.value, b.value
    else
        error'typecheck'
    end
    push(a>b)
  end,
  ge = function()
    local b, a = pop() a = pop()
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        error'typecheck'
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        error'typecheck'
      end
      a, b = a.value, b.value
    else
        error'typecheck'
    end
    push(a>=b)
  end,
  le = function()
    local b, a = pop() a = pop()
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        error'typecheck'
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        error'typecheck'
      end
      a, b = a.value, b.value
    else
        error'typecheck'
    end
    push(a<=b)
  end,
  lt = function()
    local b, a = pop() a = pop()
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        error'typecheck'
      end
    elseif ta == 'table' and a.kind == 'string' then
      if tb ~= 'table' or b.kind ~= 'string' then
        error'typecheck'
      end
      a, b = a.value, b.value
    else
      error'typecheck'
    end
    push(a<b)
  end,

  add = function()
    local b, a = pop_num() a = pop_num()
    push(a+b)
  end,
  sub = function()
    local b, a = pop_num() a = pop_num()
    push(a-b)
  end,
  mul = function()
    local b, a = pop_num() a = pop_num()
    push(a*b)
  end,
  div = function()
    local b, a = pop_num() a = pop_num()
    push(a/b)
  end,
  mod = function()
    local b, a = pop_num() a = pop_num()
    push(a%b)
  end,
  exp = function()
    local b, a = pop_num() a = pop_num()
    push(a^b)
  end,
  sqrt = function()
    push(math.sqrt(pop_num()))
  end,
  sin = function()
    push(math.sin(math.rad(pop_num())))
  end,
  cos = function()
    push(math.cos(math.rad(pop_num())))
  end,
  atan = function()
    local b, a = pop_num() a = pop_num()
    local res = math.deg(math.atan(a, b))
    if res < 0 then res = res + 360 end
    push(res)
  end,
  abs = function()
    push(math.abs(pop_num()))
  end,
  neg = function()
    push(-pop_num())
  end,
  round = function()
    return push(math.floor(pop_num()+.5))
  end,
  ceiling = function()
    return push(math.ceil(pop_num()))
  end,
  cvi = function()
    local a = pop()
    if type(a) == 'table' and a.kind == 'executable' then
      a = a.value
    end
    if type(a) == 'table' and a.kind == 'string' then
      a = assert((number * -1):match(a.value), 'syntaxerror')
    end
    if type(a) ~= 'number' then error'typecheck' end
    push(a//1)
  end,
  cvs = function()
    local old_str = pop_string()
    local a = pop()
    local ta = type(a)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value
      ta = type(a)
    end
    if ta == 'string' then
    elseif ta == 'boolean' then
      a = a and 'true' or 'false'
    elseif ta == 'number' then
      a = tostring(a)
    elseif ta == 'function' then
      texio.write_nl'Warning: cvs on operators is unsupported. Replaced by dummy.'
      a = '--nostringval--'
    elseif ta == 'table' then
      local kind = a.kind
      if kind == 'string' or kind == 'name' then
        a = a.value
      elseif kind == 'operator' then
        texio.write_nl'Warning: cvs on operators is unsupported. Replaced by dummy.'
        a = '--nostringval--'
      else
        a = '--nostringval--'
      end
    else
      assert(false)
    end
    if #old_str.value < #a then error'rangecheck' end
    old_str.value = a .. string.sub(old_str.value, #a+1, -1)
    return push{kind = 'string', value = a}
  end,

  string = function()
    push{kind = 'string', value = string.rep('\0', pop_int())}
  end,

  array = function()
    local size = pop_int()
    local arr = lua.newtable(size, 0)
    for i=1, size do arr[i] = null end
    push{kind = 'array', value = arr}
  end,
  astore = function()
    local arr = pop_array()
    local size = #arr.value
    for i=size, 1, -1 do
      arr.value[i] = pop()
    end
    push(arr)
  end,
  aload = function()
    local arr = pop_array()
    table.move(arr.value, 1, #arr.value, #operand_stack + 1, operand_stack)
    push(arr)
  end,

  dict = function()
    local size = pop_int()
    push{kind = 'dict', value = lua.newtable(0, size)}
  end,
  begin = function()
    dictionary_stack[#dictionary_stack + 1] = pop_dict()
  end,
  ['end'] = function()
    if #dictionary_stack <= 3 then
      error'dictstackunderflow'
    end
    dictionary_stack[#dictionary_stack] = nil
  end,
  bind = function()
    local d = pop()
    push(d) push(d)
    bind(pop_proc())
  end,
  def = function()
    local value = pop()
    local key = pop_key()
    dictionary_stack[#dictionary_stack].value[key] = value
  end,
  store = function()
    local value = pop()
    local key = pop_key()
    for i=#dictionary_stack, 1, -1 do
      if dictionary_stack[i].value[key] ~= nil then
        dictionary_stack[i].value[key] = value
        return
      end
    end
    dictionary_stack[#dictionary_stack].value[key] = value
  end,
  known = function()
    local key = pop_key()
    local dict = pop()
    push(dict.value[key] ~= nil)
  end,
  load = function()
    push(lookup(pop_key()))
  end,
  get = function()
    local key = pop()
    local obj = pop()
    if type(obj) ~= 'table' then error'typecheck' end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then error'typecheck' end
    end
    local val = obj.value
    if obj.kind == 'string' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then error'rangecheck' end
      push(string.byte(val, key+1))
    elseif obj.kind == 'array' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then error'rangecheck' end
      push(val[key])
    elseif obj.kind == 'dict' then
      push(key) key = pop_key()
      push(val[key])
    else
      error'typecheck'
    end
  end,
  put = function()
    local value = pop()
    local key = pop()
    local obj = pop()
    if type(obj) ~= 'table' then error'typecheck' end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then error'typecheck' end
    end
    local val = obj.value
    if obj.kind == 'string' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then error'rangecheck' end
      push(value) value = pop_int()
      obj.value = string.sub(val, 1, key) .. string.byte(value) .. string.sub(val, key+2, #val)
    elseif obj.kind == 'array' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then error'rangecheck' end
      val[key] = value
    elseif obj.kind == 'dict' then
      push(key) key = pop_key()
      val[key] = value
    else
      error'typecheck'
    end
  end,
  length = function()
    local obj = pop()
    if type(obj) == 'string' then
      return push(#obj)
    elseif type(obj) ~= 'table' then
      error'typecheck'
    end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then error'typecheck' end
    end
    local val = obj.value
    if obj.kind == 'string' then
      push(#val)
    elseif obj.kind == 'array' then
      push(#val)
    elseif obj.kind == 'dict' then
      local length = 0
      for _ in next, val do
        length = length + 1
      end
      push(length)
    else
      error'typecheck'
    end
  end,

  matrix = function()
    push{kind = 'array', value = {1, 0, 0, 1, 0, 0}}
  end,
  currentmatrix = function()
    local m = pop_array()
    assert(#m.value == 6)
    table.move(graphics_stack[#graphics_stack].matrix, 1, 6, 1, m.value)
    push(m)
  end,
  currentlinewidth = function()
    push(assert(graphics_stack[#graphics_stack].linewidth, 'linewidth has to be set before it is queried'))
  end,
  setlinewidth = function()
    local lw = pop_num()
    graphics_stack[#graphics_stack].linewidth = lw
    pdfprint(string.format('%.3f w', lw))
  end,
  setlinejoin = function()
    local linejoin = pop_int()
    graphics_stack[#graphics_stack].linejoin = linejoin
    pdfprint(string.format('%i j', linejoin))
  end,
  setlinecap = function()
    local linecap = pop_int()
    graphics_stack[#graphics_stack].linecap = linecap
    pdfprint(string.format('%i J', linecap))
  end,
  setstrokeadjust = function()
    local sa = pop_bool()
    graphics_stack[#graphics_stack].strokeadjust = sa
    -- TODO: PDF Instructions
  end,
  currentpoint = function()
    local current_point = assert(graphics_stack[#graphics_stack].current_point, 'nocurrentpoint')
    push(current_point[1])
    push(current_point[2])
  end,

  moveto = function()
    local y = pop_num()
    local x = pop_num()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if current_path then
      local i = #current_path + 1
      current_path[i], current_path[i+1], current_path[i+2] = x, y, 'm'
      local current_point = state.current_point
      current_point[1], current_point[2] = x, y
    else
      state.current_path = {x, y, 'm'}
      state.current_point = {x, y}
    end
  end,
  rmoveto = function()
    local y = pop_num()
    local x = pop_num()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local current_point = state.current_point
    x, y = current_point[1] + x, current_point[2] + y
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2] = x, y, 'm'
    current_point[1], current_point[2] = x, y
  end,
  lineto = function()
    local y = pop_num()
    local x = pop_num()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2] = x, y, 'l'
    local current_point = state.current_point
    current_point[1], current_point[2] = x, y
  end,
  rlineto = function()
    local y = pop_num()
    local x = pop_num()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local current_point = state.current_point
    x, y = x + current_point[1], y + current_point[2]
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2] = x, y, 'l'
    current_point[1], current_point[2] = x, y
  end,
  curveto = function()
    local y3 = pop_num()
    local x3 = pop_num()
    local y2 = pop_num()
    local x2 = pop_num()
    local y1 = pop_num()
    local x1 = pop_num()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2], current_path[i+3], current_path[i+4], current_path[i+5], current_path[i+6] = x1, y1, x2, y2, x3, y3, 'c'
    local current_point = state.current_point
    current_point[1], current_point[2] = x3, y3
  end,

  arc = function()
    local a2 = pop_num()
    local a1 = pop_num()
    local r = pop_num()
    local yc = pop_num()
    local xc = pop_num()
    while a2 < a1 do
      a2 = a2 + 360
    end
    a1, a2 = math.rad(a1), math.rad(a2)
    local dx, dy = r*math.cos(a1), r*math.sin(a1)
    local x, y = xc + dx, yc + dy
    local segments = math.ceil((a2-a1)/(math.pi*.5))
    local da = (a2-a1)/segments
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    local i
    if current_path then
      i = #current_path + 1
      current_path[i], current_path[i+1], current_path[i+2] = x, y, 'l'
      i = i + 3
    else
      current_path = {x, y, 'm'}
      i = 4
      state.current_path = current_path
      state.current_point = {}
    end
    local factor = 4*math.tan(da/4)/3
    dx, dy = factor*dy, -factor*dx
    for _=1, segments do
      current_path[i], current_path[i+1] = x - dx, y - dy
      a1 = a1 + da
      dx, dy = r*math.cos(a1), r*math.sin(a1)
      x, y = xc + dx, yc + dy
      dx, dy = factor*dy, -factor*dx
      current_path[i+2], current_path[i+3] = x + dx, y + dy
      current_path[i+4], current_path[i+5] = x, y
      current_path[i+6] = 'c'
      i = i + 7
    end
    state.current_point[1], state.current_point[2] = x, y
  end,

  clip = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    current_path[#current_path+1] = 'W n'
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        current_path[i] = string.format('%.3f', current_path[i])
      end
    end
    pdfprint(table.concat(current_path, ' '))
  end,
  fill = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    current_path[#current_path+1] = 'f'
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        current_path[i] = string.format('%.3f', current_path[i])
      end
    end
    pdfprint(table.concat(current_path, ' '))
    state.current_path, state.current_point = nil
  end,
  stroke = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    current_path[#current_path+1] = 'S'
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        current_path[i] = string.format('%.3f', current_path[i])
      end
    end
    pdfprint(table.concat(current_path, ' '))
    state.current_path, state.current_point = nil
  end,

  scale = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      if #m ~= 6 then error'Unexpected size of matrix' end
      local y = pop_num()
      local x = pop_num()
      m[1], m[2], m[3], m[4], m[5], m[6] = x, 0, 0, y, 0, 0
      push(m)
    else
      push(m)
      local y = pop_num()
      local x = pop_num()
      update_matrix(x, 0, 0, y, 0, 0)
    end
  end,
  translate = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      if #m ~= 6 then error'Unexpected size of matrix' end
      local y = pop_num()
      local x = pop_num()
      m[1], m[2], m[3], m[4], m[5], m[6] = 1, 0, 0, 1, x, y
      push(m)
    else
      push(m)
      local y = pop_num()
      local x = pop_num()
      update_matrix(1, 0, 0, 1, x, y)
    end
  end,
  rotate = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      if #m ~= 6 then error'Unexpected size of matrix' end
      local angle = math.rad(pop_num())
      local s, c = math.sin(angle), math.cos(angle)
      m[1], m[2], m[3], m[4], m[5], m[6] = c, s, -s, c, 0, 0
      push(m)
    else
      push(m)
      local angle = math.rad(pop_num())
      local s, c = math.sin(angle), math.cos(angle)
      update_matrix(c, s, -s, c, 0, 0)
    end
  end,
  transform = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      if #m ~= 6 then error'Unexpected size of matrix' end
    else
      push(m)
      m = graphics_stack[#graphics_stack].matrix
    end
    local y = pop_num()
    local x = pop_num()
    x, y = matrix_transform(x, y, m[1], m[2], m[3], m[4], m[5], m[6])
    push(x)
    push(y)
  end,
  itransform = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      if #m ~= 6 then error'Unexpected size of matrix' end
    else
      push(m)
      m = graphics_stack[#graphics_stack].matrix
    end
    local y = pop_num()
    local x = pop_num()
    x, y = matrix_transform(x, y, matrix_invert(m[1], m[2], m[3], m[4], m[5], m[6]))
    push(x)
    push(y)
  end,
  -- setmatrix is not supported in PDF, so we invert the old matrix first
  setmatrix = function()
    local m = pop()
    if type(m) ~= 'table' or m.kind ~= 'array' then
      error'typecheck'
    end
    local m = m.value
    if #m ~= 6 then error'rangecheck' end
    local old = graphics_stack[#graphics_stack].matrix
    local pt = graphics_stack[#graphics_stack].current_point
    local a, b, c, d, e, f = matrix_invert(old[1], old[2], old[3], old[4], old[5], old[6])
    update_matrix(a, b, c, d, e, f)
    update_matrix(m[1], m[2], m[3], m[4], m[5], m[6])
  end,
  setpdfcolor = function()
    local pdf = pop_string()
    local color = graphics_stack[#graphics_stack].color
    color.space = 'PDF'
    for i=2, #color do color[i] = nil end
    color[1] = pdf
    pdfprint(pdf.value)
  end,
  setgray = function()
    local g = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = 'Gray'
    for i=2, #color do color[i] = nil end
    color[1] = g
    -- TODO: PDF instructions
  end,
  setrgbcolor = function()
    local b = pop_num()
    local g = pop_num()
    local r = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = 'RGB'
    for i=4, #color do color[i] = nil end
    color[1], color[2], color[3] = r, g, b
    -- TODO: PDF instructions
  end,
  setcmykcolor = function()
    local k = pop_num()
    local y = pop_num()
    local m = pop_num()
    local c = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = 'CMYK'
    for i=5, #color do color[i] = nil end
    color[1], color[2], color[3], color[3] = c, m, y, k
    -- TODO: PDF instructions
  end,
  ['.setopacityalpha'] = function()
    error'Unsupported, use .setfillconstantalpha instead'
    -- TODO: PDF instructions
  end,
  ['.setfillconstantalpha'] = function()
    local alpha = pop_num()
    graphics_stack[#graphics_stack].fillconstantalpha = alpha
    -- TODO: PDF instructions
  end,
  ['.setstrokeconstantalpha'] = function()
    local alpha = pop_num()
    graphics_stack[#graphics_stack].strokeconstantalpha = alpha
    -- TODO: PDF instructions
  end,
  newpath = function()
    local state = graphics_stack[#graphics_stack]
    state.current_point = nil
    state.current_path = nil
  end,

  gsave = function()
    graphics_stack[#graphics_stack+1] = table.copy(graphics_stack[#graphics_stack])
    pdfprint'q'
  end,
  grestore = function()
    graphics_stack[#graphics_stack] = nil
    pdfprint'Q'
  end,

  setglobal = pop_bool,

  flush = function()
    io.stdout:flush()
  end,
  print = function()
    local msg = pop_string()
    io.stdout:write(msg.value)
  end,
  stack = function()
    texio.write_nl'Incompatible format for stack'
    for i=#operand_stack, 1, -1 do
      table.print(operand_stack[i])
    end
  end,

  -- TODO: Maybe someday
  stringwidth = function()
    pop_string()
    push(0)
    push(0)
    texio.write_nl'Font support is not implemented'
  end,
  show = function()
    pop_string()
    texio.write_nl'Font support is not implemented'
  end,
  definefont = function()
    local fontdict = pop_dict()
    pop_key()
    push(fontdict)
    -- texio.write_nl'Font support is not implemented'
    -- No need to warn yet since every use will trigger the warning
  end,
  scalefont = function()
    local factor = pop_num()
    -- local fontdict = pop_dict()
    -- push(fontdict)
    -- texio.write_nl'Font support is not implemented'
  end,
  setfont = function()
    local fontdict = pop_dict()
    texio.write_nl'Font support is not implemented'
  end,
  findfont = function()
    local fontname = pop_key()
    print('Looking for font', fontname)
    texio.write_nl'Font support is not implemented'
    push{kind = 'dict', value = {}}
    -- error[[invalidfont]]
  end,

  realtime = function()
    push(os.gettimeofday() * 1000 // 1)
  end,

  rrand = function()
    error'Not supported'
  end,
  srand = function()
    math.randomseed(pop_int())
  end,
  rand = function()
    push(math.random(0, 0xFFFFFFFF))
  end,

  cvx = function()
    local a = pop()
    local ta = type(a)
    if (ta == 'table' and a.kind == 'executable') or ta == 'string' or ta == 'function' then
      return push(a)
    else
      return push{kind = 'executable', value = a}
    end
  end,
  exec = function()
    return execute_tok(pop())
  end,
  stopped = function()
    local proc = pop()
    local coro = coroutine.wrap(execute_tok)
    local result = coro(proc)
    if result == 'stop' then
      push(true)
    elseif result == nil then
      push(false)
    elseif result == 'exit' then
      error'exit outside of loop'
    else
      error'???'
    end
  end,
  stop = function()
    coroutine.yield'stop'
  end,
  exit = function()
    coroutine.yield'exit'
  end,

  revision = 1000,
  ['true'] = true,
  ['false'] = false,
  systemdict = systemdict,
  globaldict = globaldict,
}}
systemdict.value.systemdict = systemdict
dictionary_stack = {systemdict, globaldict, userdict}
-- local execution_stack = {} -- Currently not implemented

function execute_tok(tok, suppress_proc)
  local ttok = type(tok)
  if ttok == 'string' then
    return execute_tok(lookup(tok))
  elseif ttok == 'function' then
    return tok()
  elseif ttok == 'table' and tok.kind == 'executable' then
    local vtok = tok.value
    ttok = type(vtok)
    if suppress_proc and ttok == 'table' and tok.value.kind == 'array' then
      return push(tok)
    end
    if ttok == 'table' then
      local kind = vtok.kind
      if kind == 'array' then
        return execute_ps(vtok.value)
      else
        error'Unimplemented'
      end
    else
      error'Unimplemented'
    end
  else
    return push(tok)
  end
end

function execute_ps(tokens)
  for i=1, #tokens do
    execute_tok(tokens[i], true)
  end
end

local func = luatexbase.new_luafunction'luaPSTheader'
token.set_lua('luaPSTheader', func, 'protected')
lua.get_functions_table()[func] = function()
  local stack_depth = #operand_stack
  local f = io.open(kpse.find_file(token.scan_argument(), 'PostScript header'), 'r')
  local src = f:read'a'
  f:close()
  local tokens = parse_ps(src)
  execute_ps(tokens)
  if #operand_stack ~= stack_depth then
    error'Unexpected values on operand stack'
  end
end

local func = luatexbase.new_luafunction'showPS'
token.set_lua('showPS', func, 'protected')
lua.get_functions_table()[func] = function()
  local command = token.scan_argument(true)
  local tokens = parse_ps(command)
  execute_ps(tokens)
  for i = 1, #operand_stack do
    local op = operand_stack[i]
    operand_stack[i] = nil
    if type(op) == 'table' then
      print(op.kind, op.value)
    else
      print(op)
    end
  end
end

--[[
local func = luatexbase.new_luafunction'pstVerb'
token.set_lua('pstVerb', func, 'protected')
lua.get_functions_table()[func] = function()
  local command = token.scan_argument(true)
  -- print('TODO: pstVerb', command)
end
]]

local ps_tokens, saved_pdfprint
local fid = font.define{
  name = 'dummy virtual font for PS rendering',
  -- type = 'virtual',
  characters = {
    [0] = {
      commands = {
        {'lua', function()
          local tokens = ps_tokens
          ps_tokens = nil
          execute_ps(tokens)
          pdfprint = saved_pdfprint -- Might help with nesting... Untested
        end}
      }
    },
  },
}

local func = luatexbase.new_luafunction'luaPST'
token.set_lua('luaPST', func, 'protected')
lua.get_functions_table()[func] = function()
  local mode = token.scan_keyword'direct' and 'page' or 'origin'
  local tokens = parse_ps(token.scan_argument(true))
  if mode == 'origin' then
    table.insert(tokens, 1, 'gsave')
    table.insert(tokens, 'grestore')
  else
    table.move(tokens, 1, #tokens, 4)
    tokens[3] = 'moveto'
  end
  local n = node.new('whatsit', 'late_lua')
  function n.data()
    if mode ~= 'origin' then
      local x, y = pdf.getpos()
      tokens[1], tokens[2] = x/65781.76, y/65781.76
    end
    assert(not ps_tokens)
    ps_tokens = tokens
    saved_pdfprint = pdfprint
    function pdfprint(s) vf.pdf(mode, s) end
  end
  local nn = node.new('glyph')
  nn.subtype = 256
  nn.font, nn.char = fid, 0
  n.next = nn
  node.write((node.hpack(n)))
end
-- luatexbase.add_to_callback('pre_shipout_filter', function(n)
--   print(n)
--   return true
-- end, 'WIP')
-- font.current(fid)
