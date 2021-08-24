-- Known incompatibilities:
--  - Many missing commands
--  - Substrings and array intervals don't share their value with the full string/array
--  - Access properties are not enforced

local pdfprint = vf.pdf -- Set later to have the right mode

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

local str_view do
  local meta = {
    __index = function(s, k)
      if k == 'value' then
        return string.sub(s.base.value, s.offset, s.last)
      end
    end,
    __newindex = function(s, k, v)
      if k == 'value' then
        s.base.value = string.sub(s.base.value, 1, s.offset-1) .. v .. string.sub(s.base.value, s.last+1)
        return
      end
      -- We could do rawset here, but there is no reason for setting keys anyway
      assert(false)
    end,
  }
  function str_view(base, offset, length)
    if getmetatable(base) == meta then
      offset = offset + base.offset - 1
      base = base.base
    end
    return setmetatable({
      kind = 'string',
      base = base,
      offset = offset,
      last = offset + length - 1,
    }, meta)
  end
end

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

local font_aliases = {
  ['NimbusSanL-Regu'] = 'zhvreg',
  ['NimbusRomNo9L-Regu'] = 'nimbusromanregular',
  ['NimbusRomNo9L-Bold'] = 'nimbusromanbold',
  ['NimbusMonL-Regu'] = 'zcolight',
  ['StandardSymL'] = 'StandardSymbolsPS',
}

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
local function pop_num()
  local n = pop()
  if type(n) == 'table' and n.kind == 'executable' then
    n = n.value
  end
  if type(n) ~= 'number' then
    error'typecheck'
  end
  return n
end
local pop_int = pop_num
local function pop_proc()
  local v = pop()
  if type(v) ~= 'table' or v.kind ~= 'executable' or type(v.value) ~= 'table' or v.value.kind ~= 'array' then
    error'typecheck'
  end
  return v.value.value
end
local pop_bool = pop
local function pop_dict()
  local dict = pop()
  if type(dict) ~= 'table' then error'typecheck' end
  if dict.kind == 'executable' then
    dict = dict.value
    if type(dict) ~= 'table' then error'typecheck' end
  end
  if dict.kind ~= 'dict' then error'typecheck' end
  return dict
end
local pop_array = pop
local pop_string = pop
local function pop_key()
  local key = pop()
  if type(key) == 'table' then
    local kind = key.kind
    if kind == 'executable' then
      key = key.value
      if type(key) ~= 'table' then return key end
      kind = key.kind
    end
    if kind == 'string' or kind == 'name' or kind == 'operator' then
      key = key.value
    end
  end
  return key
end

local execute_ps, execute_tok

local dictionary_stack
local graphics_stack = {{
  matrix = {72, 0, 0, 72, 0, 0},
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
  font = nil,
  dash = nil,
  delayed_start = nil,
}}

local char_width_storage -- Non nil only at the beginning of a Type 3 glyph. Used to export the width.
local ExtGStateCount = 0
local pdfdict_gput = token.create'pdfdict_gput:nnn'
if pdfdict_gput.cmdname == 'undefined_cs' then
  pdfdict_gput = nil
end
local lbrace = token.create(string.byte'{')
local rbrace = token.create(string.byte'}')
local ExtGState = setmetatable({}, {__index = pdfdict_gput and function(t, k)
  ExtGStateCount = ExtGStateCount + 1
  local name = 'PSExtG' .. ExtGStateCount
  tex.runtoks(function()
    tex.write(pdfdict_gput, lbrace, 'g__pdf_Core/Page/Resources/ExtGState', rbrace, lbrace, name, rbrace, lbrace, k, rbrace)
  end)
  ltx.__pdf.Page.Resources.ExtGState = true
  ltx.pdf.Page_Resources_gpush(tex.count.g_shipout_readonly_int)
  name = '/' .. name .. ' gs'
  t[k] = name
  return name
end or function()
  texio.write_nl"Extended graphic state modifications dropped since `pdfmanagement-testphase' is not loaded."
  return ''
end})

local function matrix_transform(x, y, xx, xy, yx, yy, dx, dy)
  return x * xx + y * yx + dx, x * xy + y * yy + dy
end
local function matrix_invert(xx, xy, yx, yy, dx, dy)
  local determinante = xx*yy - xy*yx
  xx, xy, yx, yy = yy/determinante, -xy/determinante, -yx/determinante, xx/determinante
  dx, dy = - dx * xx - dy * yx, - dx * xy - dy * yy
  return xx, xy, yx, yy, dx, dy
end
local delayed_text = {}
local delayed_matrix = {1, 0, 0, 1, 0, 0}
local function update_matrix(xx, xy, yx, yy, dx, dy)
  local matrix = graphics_stack[#graphics_stack].matrix
  matrix[1], matrix[2],
  matrix[3], matrix[4],
  matrix[5], matrix[6]
    = xx * matrix[1] + xy * matrix[3],             xx * matrix[2] + xy * matrix[4],
      yx * matrix[1] + yy * matrix[3],             yx * matrix[2] + yy * matrix[4],
      dx * matrix[1] + dy * matrix[3] + matrix[5], dx * matrix[2] + dy * matrix[4] + matrix[6]

  delayed_matrix[1], delayed_matrix[2],
  delayed_matrix[3], delayed_matrix[4],
  delayed_matrix[5], delayed_matrix[6]
    = xx * delayed_matrix[1] + xy * delayed_matrix[3],                     xx * delayed_matrix[2] + xy * delayed_matrix[4],
      yx * delayed_matrix[1] + yy * delayed_matrix[3],                     yx * delayed_matrix[2] + yy * delayed_matrix[4],
      dx * delayed_matrix[1] + dy * delayed_matrix[3] + delayed_matrix[5], dx * delayed_matrix[2] + dy * delayed_matrix[4] + delayed_matrix[6]

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

local function delayed_print(str)
  delayed_text[#delayed_text + 1] = str
end

local function reset_delayed(str)
  for i=1, #delayed_text do
    delayed_text[i] = nil
  end
  delayed_matrix[1], delayed_matrix[2],
  delayed_matrix[3], delayed_matrix[4],
  delayed_matrix[5], delayed_matrix[6] = 1, 0, 0, 1, 0, 0
end

local function flush_delayed(force_start)
  local cm_string = string.format('%.4f %.4f %.4f %.4f %.4f %.4f cm', delayed_matrix[1], delayed_matrix[2],
                                                                      delayed_matrix[3], delayed_matrix[4],
                                                                      delayed_matrix[5], delayed_matrix[6])
  if cm_string == "1.0000 0.0000 0.0000 1.0000 0.0000 0.0000 cm" then
    cm_string = nil
  end
  if (cm_string or delayed_text[1] or force_start) and graphics_stack[#graphics_stack].delayed_start then
    graphics_stack[#graphics_stack].delayed_start = nil
    pdfprint'q'
  end
  for i=1, #delayed_text do
    pdfprint(delayed_text[i])
  end
  if cm_string then
    pdfprint(cm_string)
  end
  return reset_delayed()
end

function drawarc(xc, yc, r, a1, a2)
  a1, a2 = math.rad(a1), math.rad(a2)
  local dx, dy = r*math.cos(a1), r*math.sin(a1)
  local x, y = xc + dx, yc + dy
  local segments = math.ceil(math.abs(a2-a1)/(math.pi*.5))
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

local setpdfcolor do
  local pdf_rgb = number * whitespace * number * whitespace * number * whitespace * 'rg'
                * whitespace * number * whitespace * number * whitespace * number * whitespace * 'RG' * -1
  local pdf_cmyk = number * whitespace * number * whitespace * number * whitespace * number * whitespace * 'k'
                 * whitespace * number * whitespace * number * whitespace * number * whitespace * number * whitespace * 'K' * -1
  local pdf_gray = number * whitespace * 'g'
                 * whitespace * number * whitespace * 'G' * -1
  function setpdfcolor(pdf, color)
    for i=1, #color do color[i] = nil end
    local r, g, b, R, G, B = pdf_rgb:match(pdf)
    if r and r == R and g == G and b == B then
      color.space = 'RGB'
      color[1], color[2], color[3] = R, G, B
      return
    end
    local c, m, y, k, C, M, Y, K = pdf_cmyk:match(pdf)
    if c and c == C and m == M and y == Y and k == K then
      color.space = 'CMYK'
      color[1], color[2], color[3], color[4] = C, M, Y, K
      return
    end
    g, G = pdf_gray:match(pdf)
    if g and g == G then
      color.space = 'Gray'
      color[1] = G
      return
    end
    color.space = 'PDF'
    color[1] = pdf
  end
end

local subdivide, flatten do
  function subdivide(t, x0, y0, x1, y1, x2, y2, x3, y3)
    local mt = 1-t
    local x01, y01 = mt * x0 + t * x1, mt * y0 + t * y1
    local x12, y12 = mt * x1 + t * x2, mt * y1 + t * y2
    local x23, y23 = mt * x2 + t * x3, mt * y2 + t * y3
    local x012, y012 = mt * x01 + t * x12, mt * y01 + t * y12
    local x123, y123 = mt * x12 + t * x23, mt * y12 + t * y23
    local x0123, y0123 = mt * x012 + t * x123, mt * y012 + t * y123
    return x01, y01, x012, y012, x0123, y0123, x123, y123, x23, y23, x3, y3
  end
  local function flatness(x0, y0, x1, y1, x2, y2, x3, y3)
    local dx, dy = x3-x0, y3-y0
    local dist = math.sqrt(dx*dx + dy*dy)
    local d1 = math.abs(dx * (x0-x1) - dy * (y0-y1)) / dist
    local d2 = math.abs(dx * (x0-x2) - dy * (y0-y2)) / dist
    return d1 > d2 and d1 or d2
  end
  function flatten(out, target, x0, y0, x1, y1, x2, y2, x3, y3)
    local current = flatness(x0, y0, x1, y1, x2, y2, x3, y3)
    if current <= target then
      local i = #out
      -- out[i+1], out[i+2],
      -- out[i+3], out[i+4],
      -- out[i+5], out[i+6], out[i+7]
      --   = x1, y1, x2, y2, x3, y3, 'c'
      out[i+1], out[i+2], out[i+3]
        = x3, y3, 'l'
      return
    end
    local a, b, c, d, e, f, g, h, i, j, k, l = subdivide(.5, x0, y0, x1, y1, x2, y2, x3, y3)
    flatten(out, target, x0, y0, a, b, c, d, e, f)
    return flatten(out, target, e, f, g, h, i, j, k, l)
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
local FontDirectory = {kind = 'dict', value = {}}
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
    local arg = pop()
    local exec
    if type(arg) == 'table' and arg.kind == 'executable' then
      exec = true
      arg = arg.value
    end
    if type(arg) == 'number' then
      local height = #operand_stack
      if arg > height then
        error'copy argument larger then stack'
      end
      table.move(operand_stack, height-arg+1, height, height+1)
    elseif type(arg) == 'table' then
      -- See remarks in getinterval about missing functionality
      local kind = arg.kind
      if kind == 'array' then
        local src = pop_array().value
        if #src ~= #arg.value then
          error'copy with different sized arrays is not implemented yet'
        end
        table.move(src, 1, #src, 1, arg.value)
      elseif kind == 'string' then
        local src = pop_string().value
        if #src == #arg.value then
        elseif #src < #arg.value then
          arg = str_view(arg, 1, #src)
        else
          error'rangecheck'
        end
        arg.value = src
      elseif kind == 'dict' then
        local src = pop_dict().value
        if next(arg.value) then
          error'Target dictionary must be empty'
        end
        for k, v in next, src do
          arg.value[k] = v
        end
      else
        error'typecheck'
      end
      push(exec and {kind = 'executable', value = arg} or arg)
    else
      error'typecheck'
    end
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
  forall = function()
    local proc = pop_proc()
    local obj = pop()
    if type(obj) ~= 'table' then error'typecheck' end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then error'typecheck' end
    end
    local coro = coroutine.wrap(
         obj.kind == 'array' and function()
           for i=1, #obj.value do
             push(obj.value[i])
             execute_ps(proc)
           end
         end
      or obj.kind == 'string' and function()
           for b in string.bytes(obj.value) do
             push(b)
             execute_ps(proc)
           end
         end
      or obj.kind == 'dict' and function()
           for k, v in next, obj.value do
             push(k)
             push(v)
             execute_ps(proc)
           end
         end
      or error'typecheck')
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
  loop = function()
    local proc = pop_proc()
    local coro = coroutine.wrap(function()
      while true do
        execute_ps(proc)
      end
    end)
    local result = coro(proc)
    if result == 'exit' or not result then
    else
      coroutine.yield(result)
    end
  end,

  pathforall = function()
    local close = pop_proc()
    local curve = pop_proc()
    local line = pop_proc()
    local move = pop_proc()
    local state = graphics_stack[#graphics_stack]
    local path = state.current_path
    if not path then return end
    path = table.move(path, 1, #path, 1, {}) -- We don't want to be affected by modifications
    local coro = coroutine.wrap( function()
      local i = 1
      while true do
        local entry = path[i]
        if type(entry) == 'string' then
          execute_ps(entry == 'm' and move or entry == 'l' and line or entry == 'c' and curve or entry == 'h' and close or error'Unexpected path operator')
        elseif entry then
          push(entry)
        else
          break
        end
        i = i + 1
      end
    end)
    local result = coro(proc)
    if result == 'exit' or not result then
    else
      coroutine.yield(result)
    end
  end,
  pathbbox = function()
    local current_path = assert(graphics_stack[#graphics_stack].current_path, 'nocurrentpoint')
    local i=1
    local llx, llyx, urx, ury
    while current_path[i] do
      local entry = current_path[i]
      if type(entry) == 'number' then
        local after = current_path[i+1]
        assert(type(after) == 'number')
        llx = llx and llx < entry and llx or entry
        lly = lly and lly < after and lly or after
        urx = urx and urx > entry and urx or entry
        ury = ury and ury > after and ury or after
        i = i+2
      else
        i = i+1
      end
    end
    push(llx)
    push(lly)
    push(urx)
    push(ury)
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
      push(pop_bool() or val)
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
  ne = function()
    local b, a = pop() a = pop()
    if type(a) == 'table' and (a.kind == 'executable' or a.kind == 'name' or a.kind == 'operator') then
      a = a.value
    end
    if type(b) == 'table' and (b.kind == 'executable' or b.kind == 'name' or b.kind == 'operator') then
      b = b.value
    end
    push(a~=b)
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
  idiv = function()
    local b, a = pop_num() a = pop_num()
    push(a//b)
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
  floor = function()
    return push(math.floor(pop_num()))
  end,
  cvn = function()
    local a = pop()
    if type(a) == 'table' and a.kind == 'executable' then
      local val = a.value
      if type(val) ~= 'table' or val.kind ~= 'string' then
        error'typecheck'
      end
      push(val.value)
    end
    if type(a) ~= 'table' or a.kind ~= 'string' then
      error'typecheck'
    end
    return push{kind = 'name', value = a.value}
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
  cvr = function()
    local a = pop()
    if type(a) == 'table' and a.kind == 'executable' then
      a = a.value
    end
    if type(a) == 'table' and a.kind == 'string' then
      a = assert((number * -1):match(a.value), 'syntaxerror')
    end
    if type(a) ~= 'number' then error'typecheck' end
    push(a*1.)
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
  getinterval = function()
    local count = pop_int()
    local index = pop_int()
    local arr = pop()
    if type(arr) ~= 'table' then error'typecheck' end
    if arr.kind == 'executable' then
      arr = arr.value
      if type(arr) ~= 'table' then error'typecheck' end
    end
    if arr.kind == 'string' then
      push(str_view(arr, index + 1, count))
    elseif arr.kind == 'array' then
      -- TODO: At least for the array case, we could use metamethods to make get element sharing behavior
      push{kind = 'array', value = table.move(arr.value, index + 1, index + count, 1, {})}
    else
      error'typecheck'
    end
  end,
  putinterval = function()
    local from = pop()
    local index = pop_int()
    if type(from) ~= 'table' then error'typecheck' end
    if from.kind == 'executable' then
      from = from.value
      if type(from) ~= 'table' then error'typecheck' end
    end
    if from.kind == 'string' then
      local to = pop_string()
      from = from.value
      to.value = string.sub(to.value, 1, index) .. from .. string.sub(to.value, index + 1 + #from)
    elseif from.kind == 'array' then
      local to = pop_array()
      table.move(from.value, 1, #from.value, index + 1, to.value)
    else
      error'typecheck'
    end
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
  currentdict = function()
    push(dictionary_stack[#dictionary_stack])
  end,
  bind = function()
    local d = pop()
    push(d)
    if type(d) ~= 'table' then error'typecheck' end
    if d.kind == 'executable' then
      d = d.value
      if type(d) ~= 'table' then error'typecheck' end
    end
    if d.kind ~= 'array' then error'typecheck' end
    bind(d.value)
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
  where = function()
    local key = pop_key()
    for i = #dictionary_stack, 1, -1 do
      local dict = dictionary_stack[i]
      local value = dict.value[key]
      if value ~= nil then
        push(dict)
        return push(true)
      end
    end
    return push(false)
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
      push(val[key+1])
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
      val[key+1] = value
    elseif obj.kind == 'dict' then
      push(key) key = pop_key()
      val[key] = value
    else
      error'typecheck'
    end
  end,
  undef = function()
    local key = pop_key()
    local dict = pop_dict().value
    dict[key] = nil
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
    delayed_print(string.format('%.3f w', lw))
  end,
  setlinejoin = function()
    local linejoin = pop_int()
    graphics_stack[#graphics_stack].linejoin = linejoin
    delayed_print(string.format('%i j', linejoin))
  end,
  setlinecap = function()
    local linecap = pop_int()
    graphics_stack[#graphics_stack].linecap = linecap
    delayed_print(string.format('%i J', linecap))
  end,
  setstrokeadjust = function()
    local sa = pop_bool()
    graphics_stack[#graphics_stack].strokeadjust = sa
    delayed_print(ExtGState[sa and '<</SA true>>' or '<</SA false>>'])
  end,
  setdash = function()
    local offset = pop_num()
    local patt = pop_array().value
    graphics_stack[#graphics_stack].dash = {offset = offset, pattern = patt}
    local mypatt = {}
    for i=1, #patt do
      mypatt[i] = string.format('%.3f', patt[i])
    end
    delayed_print(string.format('[%s] %.3f d', table.concat(mypatt, ' '), offset))
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
  closepath = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    local current_point = state.current_point
    if not current_path then return end
    local x, y
    for i=#current_path, 1, -1 do
      if current_path[i] == 'm' then
        x, y = assert(tonumber(current_path[i-2])), assert(tonumber(current_path[i-1]))
      end
    end
    current_point[1], current_point[2] = assert(x), y
    current_path[#current_path + 1] = 'h'
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
    drawarc(xc, yc, r, a1, a2)
  end,
  arcn = function()
    local a2 = pop_num()
    local a1 = pop_num()
    local r = pop_num()
    local yc = pop_num()
    local xc = pop_num()
    while a1 < a2 do
      a1 = a1 + 360
    end
    drawarc(xc, yc, r, a1, a2)
  end,
  arcto = function()
    local r = pop_num()
    local y2 = pop_num()
    local x2 = pop_num()
    local y1 = pop_num()
    local x1 = pop_num()

    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local current_point = state.current_point
    local x0, y0 = current_point[1], current_point[2]

    local dx1, dy1 = x1 - x0, y1 - y0
    local dx2, dy2 = x2 - x1, y2 - y1

    local a1 = math.atan(dy1, dx1)
    local a2 = math.atan(dy2, dx2)

    if a1 - math.pi > a2 then
      a1 = a1 - 2*math.pi
    elseif a2 - math.pi > a1 then
      a2 = a2 - 2*math.pi
    end

    if a1 > a2 then
      a1 = a1 + math.pi/2
      a2 = a2 + math.pi/2
    else
      a1 = a1 - math.pi/2
      a2 = a2 - math.pi/2
    end

    local ox1, oy1 = r * math.cos(a1), r * math.sin(a1)
    local ox2, oy2 = r * math.cos(a2), r * math.sin(a2)
    -- Now we need to calculate the intersection of the lines offset by o1/o2
    -- to determine the center. We inlin eth ematix inverse for performance and better handling of edge cases.
    -- local t1, t2 = matrix_transform(0, 0, matrix_invert(dx1, dy1, dx2, dy2, ox2-ox1, oy2-oy1))
    local det = dx1*dy2 - dy1*dx2
    if math.abs(det) < 0.0000001 then
      -- Just draw a line
      push(x1)
      push(y1)
      systemdict.value.lineto()
      push(x1)
      push(y1)
      push(x1)
      push(y1)
      return
    end
    local t1 = (ox1 - ox2) * dy2/det + (oy2 - oy1) * dx2/det
    local cx, cy = x1 - ox1 + t1 * dx1, y1 - oy1 + t1 * dy1
    -- local ccx, ccy = x1 - ox2 - t2 * dx2, y1 - oy2 + t2 * dy2
    drawarc(cx, cy, r, a1*180/math.pi, a2*180/math.pi)

    push(cx + ox1)
    push(cy + oy1)
    push(cx + ox2)
    push(cy + oy2)
  end,
  arct = function()
    systemdict.value.arcto()
    pop()
    pop()
    pop()
    pop()
  end,

  clip = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    flush_delayed(true)
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        pdfprint(string.format('%.3f', current_path[i]))
      else
        pdfprint(current_path[i])
      end
    end
    pdfprint'W n'
  end,
  eofill = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    current_path[#current_path+1] = 'f*'
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        current_path[i] = string.format('%.3f', current_path[i])
      end
    end
    flush_delayed()
    pdfprint(table.concat(current_path, ' '))
    state.current_path, state.current_point = nil
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
    flush_delayed()
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
    flush_delayed()
    pdfprint(table.concat(current_path, ' '))
    state.current_path, state.current_point = nil
  end,
  flattenpath = function()
    local state = graphics_stack[#graphics_stack]
    local old_path = assert(state.current_path, 'nocurrentpoint')
    local new_path = {}
    local last_x, last_y = nil, 0
    local saved_x, saved_y
    local subpath_x, subpath_y
    local last_op = 1
    for i=1, #old_path do
      local entry = old_path[i]
      if type(entry) == 'string' then
        if entry == 'c' then
          assert(i - last_op == 6)
          flatten(new_path, 1, saved_x, saved_y, table.unpack(old_path, last_op, i-1))-- TODO Replace 1 with flatten graphic state parameter
          table.move(old_path, last_op + 4, last_op + 5, #new_path + 1, new_path)
          new_path[#new_path+1] = 'l'
        else
          if entry == 'm' then
            subpath_x, subpath_y = last_x, last_y
          elseif entry == 'h' then
            last_x, last_y = subpath_x, subpath_y
          end
          table.move(old_path, last_op, i, #new_path + 1, new_path)
        end
        saved_x, saved_y = last_x, last_y
        last_op = i + 1
      else
        if last_y then
          last_x, last_y = entry
        else
          last_y = entry
        end
      end
    end
    assert(last_op == #old_path + 1)
    state.current_path = new_path
  end,

  scale = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      local mv = m.value
      if #mv ~= 6 then error'Unexpected size of matrix' end
      local y = pop_num()
      local x = pop_num()
      mv[1], mv[2], mv[3], mv[4], mv[5], mv[6] = x, 0, 0, y, 0, 0
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
      local mv = m.value
      if #mv ~= 6 then error'Unexpected size of matrix' end
      local y = pop_num()
      local x = pop_num()
      mv[1], mv[2], mv[3], mv[4], mv[5], mv[6] = 1, 0, 0, 1, x, y
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
      local mv = m.value
      if #mv ~= 6 then error'Unexpected size of matrix' end
      local angle = math.rad(pop_num())
      local s, c = math.sin(angle), math.cos(angle)
      mv[1], mv[2], mv[3], mv[4], mv[5], mv[6] = c, s, -s, c, 0, 0
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
      m = m.value
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
      m = m.value
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
  dtransform = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      m = m.value
      if #m ~= 6 then error'Unexpected size of matrix' end
    else
      push(m)
      m = graphics_stack[#graphics_stack].matrix
    end
    local y = pop_num()
    local x = pop_num()
    x, y = matrix_transform(x, y, m[1], m[2], m[3], m[4], 0, 0)
    push(x)
    push(y)
  end,
  idtransform = function()
    local m = pop()
    if type(m) == 'table' and m.kind == 'array' then
      m = m.value
      if #m ~= 6 then error'Unexpected size of matrix' end
    else
      push(m)
      m = graphics_stack[#graphics_stack].matrix
    end
    local y = pop_num()
    local x = pop_num()
    x, y = matrix_transform(x, y, matrix_invert(m[1], m[2], m[3], m[4], 0, 0))
    push(x)
    push(y)
  end,
  concatmatrix = function()
    local m3a = pop_array()
    local m3 = m3a.value
    if #m3 ~= 6 then error'Unexpected size of matrix' end
    local m2 = pop_array().value
    if #m2 ~= 6 then error'Unexpected size of matrix' end
    local m1 = pop_array().value
    if #m1 ~= 6 then error'Unexpected size of matrix' end
    m3[1], m3[2],
    m3[3], m3[4],
    m3[5], m3[6]
      = m1[1] * m2[1] + m1[2] * m2[3],         m1[1] * m2[2] + m1[2] * m2[4],
        m1[3] * m2[1] + m1[4] * m2[3],         m1[3] * m2[2] + m1[4] * m2[4],
        m1[5] * m2[1] + m1[6] * m2[3] + m2[5], m1[5] * m2[2] + m1[6] * m2[4] + m2[6]
    push(m3a)
  end,
  concat = function()
    local m = pop_array()
    if #m3 ~= 6 then error'Unexpected size of matrix' end
    update_matrix(m[1], m[2], m[3], m[4], m[5], m[6])
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
    local pdf = pop_string().value
    delayed_print(pdf)
    return setpdfcolor(pdf, graphics_stack[#graphics_stack].color)
  end,
  setgray = function()
    local g = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = 'Gray'
    for i=2, #color do color[i] = nil end
    color[1] = g
    delayed_print(string.format('%.3f g %.3f G', g, g))
  end,
  setrgbcolor = function()
    local b = pop_num()
    local g = pop_num()
    local r = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = 'RGB'
    for i=4, #color do color[i] = nil end
    color[1], color[2], color[3] = r, g, b
    delayed_print(string.format('%.3f %.3f %.3f rg %.3f %.3f %.3f RG', r, g, b, r, g, b))
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
    delayed_print(string.format('%.3f %.3f %.3f %.3f k %.3f %.3f %.3f %.3f K', c, m, y, k, c, m, y, k))
  end,
  ['.setopacityalpha'] = function()
    error'Unsupported, use .setfillconstantalpha instead'
  end,
  ['.setfillconstantalpha'] = function()
    local alpha = pop_num()
    graphics_stack[#graphics_stack].fillconstantalpha = alpha
    delayed_print(ExtGState['<</ca ' .. alpha .. '>>'])
  end,
  ['.setstrokeconstantalpha'] = function()
    local alpha = pop_num()
    graphics_stack[#graphics_stack].strokeconstantalpha = alpha
    delayed_print(ExtGState['<</CA ' .. alpha .. '>>'])
  end,
  newpath = function()
    local state = graphics_stack[#graphics_stack]
    state.current_point = nil
    state.current_path = nil
  end,

  currentrgbcolor = function()
    local r
    local g
    local b
    local color = graphics_stack[#graphics_stack].color
    if not color then error'Color has to be set before it is queried' end
    local space = color.space
    if space == 'RGB' then
      r, g, b = color[1], color[2], color[3]
    elseif space == 'Gray' then
      r = color[1]
      g, b = r, r
    elseif space == 'CMYK' then
      local c, m, y, k = color[1], color[2], color[3], color[4]
      c, m, y = c+k, m+k, y+k
      r, g, b = c >= 1 and 0 or 1-c, m >= 1 and 0 or 1-m, y >= 1 and 0 or 1-y
    elseif space == 'PDF' then
      r, g, b = 0, 0, 0
      print('???', 'torgb', color[1])
    else
      r, g, b = 0, 0, 0
    end
    push(r)
    push(g)
    push(b)
  end,
  currentfont = function()
    local f = graphics_stack[#graphics_stack].font
    if f then
      push(f)
    else
      push{kind = 'dict', value = {
         FID = font.current(),
         FontMatrix = {kind = 'array', value = {1, 0, 0, 1, 0, 0}},
         FontName = {kind = 'name', value = tex.fontname(font.current())},
         FontType = 0x1CA,
       }}
    end
  end,

  gsave = function()
    flush_delayed()
    graphics_stack[#graphics_stack+1] = table.copy(graphics_stack[#graphics_stack])
    graphics_stack[#graphics_stack].delayed_start = true
  end,
  grestore = function()
    if not graphics_stack[#graphics_stack].delayed_start then
      pdfprint'Q'
    end
    graphics_stack[#graphics_stack] = nil
    reset_delayed()
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

  stringwidth = function()
    local str = pop_string().value
      local state = graphics_stack[#graphics_stack]
    local psfont = assert(state.font, 'invalidfont').value
    local fid = psfont.FID
    local matrix = psfont.FontMatrix.value
    local fonttype = psfont.FontType
    if fonttype ~= 0x1CA then
      texio.write_nl'Font support is not implemented'
      return
    end
    local characters = assert(font.getfont(fid)).characters
    local w = 0
    for b in string.bytes(str) do
      local char = characters[b]
      w = w + (char and char.width or 0)
    end
    local x, y = matrix_transform(w/65781.76, 0,
      matrix[1], matrix[2],
      matrix[3], matrix[4],
      0, 0)
    push(x)
    push(y)
  end,
  show = function()
    local str = pop_string().value
    local state = graphics_stack[#graphics_stack]
    local current_point = assert(state.current_point, 'nocurrentpoint')
    local rawpsfont = assert(state.font, 'invalidfont')
    local psfont = rawpsfont.value
    local fid = psfont.FID
    local matrix = psfont.FontMatrix.value
    local fonttype = psfont.FontType
    if fonttype ~= 0x1CA and fonttype ~= 3 then
      texio.write_nl'Font support is not implemented'
      return
    end
    update_matrix(
      matrix[1],                    matrix[2],
      matrix[3],                    matrix[4],
      matrix[5] + current_point[1], matrix[6] + current_point[2])
    local w = 0
    if fonttype == 0x1CA then
      local characters = assert(font.getfont(fid)).characters
      flush_delayed()
      vf.push()
      vf.fontid(fid)
      for b in string.bytes(str) do
        vf.char(b)
        local char = characters[b]
        w = w + (char and char.width or 0)
      end
      vf.pop()
    elseif fonttype == 3 then
      local w = 0
      for b in string.bytes(str) do
        systemdict.value.gsave()
        local state = graphics_stack[#graphics_stack]
        state.current_point, state.current_path = nil
        push(rawpsfont)
        push(b)
        local this_w
        char_width_storage = function(width)
          this_w = width
        end
        execute_tok(psfont.BuildChar) -- FIXME(maybe): Switch to BuildGlyph?
        systemdict.value.grestore()
        w = w + assert(this_w, 'Type 3 character failed to set width')
        update_matrix(1, 0, 0, 1, this_w, 0)
      end
      update_matrix(1, 0, 0, 1, -w, 0)
    else
      assert(false)
    end
    push(w/65781.76)
    push(0)
    systemdict.value.rmoveto()
    update_matrix(matrix_invert(
      matrix[1],                    matrix[2],
      matrix[3],                    matrix[4],
      matrix[5] + current_point[1], matrix[6] + current_point[2]))
  end,
  definefont = function()
    local fontdict = pop_dict()
    local fontkey = pop_key()
    fontdict.value.FontMatrix = fontdict.value.FontMatrix or {kind = 'array', value = {1, 0, 0, 1, 0, 0}}
    if assert(fontdict.value.FontType) == 0x1CA then
      local fontname = fontdict.value.FontName
      if type(fontname) == 'table' and fontname.kind == 'name' then
        fontname = fontname.value
      elseif type(fontname) ~= 'string' then
        error'typecheck'
      end
      local fid = fonts.definers.read(fontname, 65782)
      if not fid then error'invalidfont' end
      if not tonumber(fid) then
        local data = fid
        fid = font.define(data)
        fonts.definers.register(data, fid)
      end
      fontdict.value.FID = fid
    else
      texio.write_nl'definefont is not implemnted. Pushing dummy font.'
    end
    FontDirectory[fontkey] = fontdict
    push(fontdict)
  end,
  makefont = function()
    local m = pop_array().value
    if #m ~= 6 then error'Unexpected size of matrix' end
    local fontdict = pop_dict()--.value
    fontdict = fontdict.value
    local new_fontdict = {}
    for k,v in next, fontdict do
      new_fontdict[k] = v
    end
    local old_m = assert(fontdict.FontMatrix, 'invalidfont').value
    new_fontdict.FontMatrix = {kind = 'array', value = {
      old_m[1] * m[1] + old_m[2] * m[3],        old_m[1] * m[2] + old_m[2] * m[4],
      old_m[3] * m[1] + old_m[4] * m[3],        old_m[3] * m[2] + old_m[4] * m[4],
      old_m[5] * m[1] + old_m[6] * m[3] + m[5], old_m[5] * m[2] + old_m[6] * m[4] + m[6],
    }}
    push{kind = 'dict', value = new_fontdict}
  end,
  scalefont = function()
    local factor = pop_num()
    local fontdict = pop_dict()--.value
    fontdict = fontdict.value
    local new_fontdict = {}
    for k,v in next, fontdict do
      new_fontdict[k] = v
    end
    local old_m = assert(fontdict.FontMatrix, 'invalidfont').value
    new_fontdict.FontMatrix = {kind = 'array', value = {
      factor * old_m[1], factor * old_m[2],
      factor * old_m[3], factor * old_m[4],
      factor * old_m[5], factor * old_m[6],
    }}
    push{kind = 'dict', value = new_fontdict}
  end,
  setfont = function()
    local fontdict = pop_dict()
    local state = graphics_stack[#graphics_stack]
    state.font = fontdict
  end,
  findfont = function()
    local fontname = pop_key()
    local fontdict = FontDirectory[fontname]
    if fontdict then push(fontdict) return end

    fontname = font_aliases[fontname] or fontname
    local fid = fonts.definers.read(fontname, 65782)
    if not fid then error'invalidfont' end
    if not tonumber(fid) then
      local data = fid
      fid = font.define(data)
      fonts.definers.register(data, fid)
    end
    return push{kind = 'dict', value = {
      FID = fid,
      FontMatrix = {kind = 'array', value = {1, 0, 0, 1, 0, 0}},
      FontName = {kind = 'name', value = fontname},
      FontType = 0x1CA,
    }}
  end,

  setcharwidth = function()
    -- Pop and ignore the advance height -- FIXME(maybe)
    pop_num()
    assert(char_width_storage, 'undefined')(pop_num())
    char_width_storage = nil
  end,
  setcachedevice = function()
    -- First pop and ignore the bounding box
    pop_num()
    pop_num()
    pop_num()
    pop_num()
    -- Fallback to setcharwidth
    systemdict.value.setcharwidth()
  end,
  setcachedevice2 = function()
    -- First pop additional entries for setccachedevice2 -- TODO: Implement other writing modes
    pop_num()
    pop_num()
    pop_num()
    pop_num()
    -- Fallback to setcachedevice
    systemdict.value.setcachedevice()
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

  type = function()
    local val = pop()
    local tval = type(val)
    if tval == 'table' and val.kind == 'executable' then
      val = val.value
      tval = type(val)
    end
    local tname
    if tval == 'string' then
      tname = 'nametype'
    elseif tval == 'number' then
      tname = math.type(val) == 'integer' and 'integertype' or 'realtype' 
    elseif tval == 'boolean' then
      tname = 'booleantype'
    elseif tval == 'function' then
      tname = 'operatortype'
    elseif tval == 'table' then
      local kind = val.kind
      if kind == 'name' then
        tname = 'nametype'
      elseif kind == 'operator' then
        tname = 'operatortype'
      elseif kind == 'array' then
        tname = 'arraytype'
      elseif kind == 'dict' then
        tname = 'dicttype'
      elseif kind == 'dict' then
        tname = 'dicttype'
      elseif kind == 'null' then
        tname = 'nulltype'
      elseif kind == 'mark' then
        tname = 'nulltype'
      elseif kind == 'string' then
        tname = 'stringtype'
      else
        assert(false, 'Unexpected type')
      end
    else
      assert(false, 'Unexpected type')
    end
    push{kind = 'name', value = tname}
    -- filetype
    -- fonttype
    -- gstatetype (LanguageLevel 2)
    -- packedarraytype (LanguageLevel 2)
    -- savetype
  end,
  xcheck = function()
    local a = pop()
    local ta = type(a)
    push(ta == 'function' or ta == 'name' or (ta == 'table' and a.kind == 'executable'))
  end,
  cvlit = function()
    local a = pop()
    local ta = type(a)
    if (ta == 'table' and a.kind == 'executable') or ta == 'string' or ta == 'function' then
      return push(a.value)
    end
    if ta == 'string' then
      return push{kind = 'name', value = a}
    end
    if ta == 'function' then
      return push{kind = 'operator', value = a}
    end
    return push(a)
  end,
  cvx = function()
    local a = pop()
    local ta = type(a)
    if (ta == 'table' and a.kind == 'executable') or ta == 'string' or ta == 'function' then
      return push(a)
    elseif ta == 'table' and (a.kind == 'operator' or a.kind == 'name') then
      return push(a.value)
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
  FontDirectory = FontDirectory,

  ISOLatin1Encoding = {kind = 'array', value = {
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = 'space'},
    {kind = 'name', value = 'exclam'},
    {kind = 'name', value = 'quotedbl'},
    {kind = 'name', value = 'numbersign'},
    {kind = 'name', value = 'dollar'},
    {kind = 'name', value = 'percent'},
    {kind = 'name', value = 'ampersand'},
    {kind = 'name', value = 'quoteright'},
    {kind = 'name', value = 'parenleft'},
    {kind = 'name', value = 'parenright'},
    {kind = 'name', value = 'asterisk'},
    {kind = 'name', value = 'plus'},
    {kind = 'name', value = 'comma'},
    {kind = 'name', value = 'minus'},
    {kind = 'name', value = 'period'},
    {kind = 'name', value = 'slash'},
    {kind = 'name', value = 'zero'},
    {kind = 'name', value = 'one'},
    {kind = 'name', value = 'two'},
    {kind = 'name', value = 'three'},
    {kind = 'name', value = 'four'},
    {kind = 'name', value = 'five'},
    {kind = 'name', value = 'six'},
    {kind = 'name', value = 'seven'},
    {kind = 'name', value = 'eight'},
    {kind = 'name', value = 'nine'},
    {kind = 'name', value = 'colon'},
    {kind = 'name', value = 'semicolon'},
    {kind = 'name', value = 'less'},
    {kind = 'name', value = 'equal'},
    {kind = 'name', value = 'greater'},
    {kind = 'name', value = 'question'},
    {kind = 'name', value = 'at'},
    {kind = 'name', value = 'A'},
    {kind = 'name', value = 'B'},
    {kind = 'name', value = 'C'},
    {kind = 'name', value = 'D'},
    {kind = 'name', value = 'E'},
    {kind = 'name', value = 'F'},
    {kind = 'name', value = 'G'},
    {kind = 'name', value = 'H'},
    {kind = 'name', value = 'I'},
    {kind = 'name', value = 'J'},
    {kind = 'name', value = 'K'},
    {kind = 'name', value = 'L'},
    {kind = 'name', value = 'M'},
    {kind = 'name', value = 'N'},
    {kind = 'name', value = 'O'},
    {kind = 'name', value = 'P'},
    {kind = 'name', value = 'Q'},
    {kind = 'name', value = 'R'},
    {kind = 'name', value = 'S'},
    {kind = 'name', value = 'T'},
    {kind = 'name', value = 'U'},
    {kind = 'name', value = 'V'},
    {kind = 'name', value = 'W'},
    {kind = 'name', value = 'X'},
    {kind = 'name', value = 'Y'},
    {kind = 'name', value = 'Z'},
    {kind = 'name', value = 'bracketleft'},
    {kind = 'name', value = 'backslash'},
    {kind = 'name', value = 'bracketright'},
    {kind = 'name', value = 'asciicircum'},
    {kind = 'name', value = 'underscore'},
    {kind = 'name', value = 'quoteleft'},
    {kind = 'name', value = 'a'},
    {kind = 'name', value = 'b'},
    {kind = 'name', value = 'c'},
    {kind = 'name', value = 'd'},
    {kind = 'name', value = 'e'},
    {kind = 'name', value = 'f'},
    {kind = 'name', value = 'g'},
    {kind = 'name', value = 'h'},
    {kind = 'name', value = 'i'},
    {kind = 'name', value = 'j'},
    {kind = 'name', value = 'k'},
    {kind = 'name', value = 'l'},
    {kind = 'name', value = 'm'},
    {kind = 'name', value = 'n'},
    {kind = 'name', value = 'o'},
    {kind = 'name', value = 'p'},
    {kind = 'name', value = 'q'},
    {kind = 'name', value = 'r'},
    {kind = 'name', value = 's'},
    {kind = 'name', value = 't'},
    {kind = 'name', value = 'u'},
    {kind = 'name', value = 'v'},
    {kind = 'name', value = 'w'},
    {kind = 'name', value = 'x'},
    {kind = 'name', value = 'y'},
    {kind = 'name', value = 'z'},
    {kind = 'name', value = 'braceleft'},
    {kind = 'name', value = 'bar'},
    {kind = 'name', value = 'braceright'},
    {kind = 'name', value = 'asciitilde'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = 'dotlessi'},
    {kind = 'name', value = 'grave'},
    {kind = 'name', value = 'acute'},
    {kind = 'name', value = 'circumflex'},
    {kind = 'name', value = 'tilde'},
    {kind = 'name', value = 'macron'},
    {kind = 'name', value = 'breve'},
    {kind = 'name', value = 'dotaccent'},
    {kind = 'name', value = 'dieresis'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = 'ring'},
    {kind = 'name', value = 'cedilla'},
    {kind = 'name', value = '.notdef'},
    {kind = 'name', value = 'hungarumlaut'},
    {kind = 'name', value = 'ogonek'},
    {kind = 'name', value = 'caron'},
    {kind = 'name', value = 'space'},
    {kind = 'name', value = 'exclamdown'},
    {kind = 'name', value = 'cent'},
    {kind = 'name', value = 'sterling'},
    {kind = 'name', value = 'currency'},
    {kind = 'name', value = 'yen'},
    {kind = 'name', value = 'brokenbar'},
    {kind = 'name', value = 'section'},
    {kind = 'name', value = 'dieresis'},
    {kind = 'name', value = 'copyright'},
    {kind = 'name', value = 'ordfeminine'},
    {kind = 'name', value = 'guillemotleft'},
    {kind = 'name', value = 'logicalnot'},
    {kind = 'name', value = 'hyphen'},
    {kind = 'name', value = 'registered'},
    {kind = 'name', value = 'macron'},
    {kind = 'name', value = 'degree'},
    {kind = 'name', value = 'plusminus'},
    {kind = 'name', value = 'twosuperior'},
    {kind = 'name', value = 'threesuperior'},
    {kind = 'name', value = 'acute'},
    {kind = 'name', value = 'mu'},
    {kind = 'name', value = 'paragraph'},
    {kind = 'name', value = 'periodcentered'},
    {kind = 'name', value = 'cedilla'},
    {kind = 'name', value = 'onesuperior'},
    {kind = 'name', value = 'ordmasculine'},
    {kind = 'name', value = 'guillemotright'},
    {kind = 'name', value = 'onequarter'},
    {kind = 'name', value = 'onehalf'},
    {kind = 'name', value = 'threequarters'},
    {kind = 'name', value = 'questiondown'},
    {kind = 'name', value = 'Agrave'},
    {kind = 'name', value = 'Aacute'},
    {kind = 'name', value = 'Acircumflex'},
    {kind = 'name', value = 'Atilde'},
    {kind = 'name', value = 'Adieresis'},
    {kind = 'name', value = 'Aring'},
    {kind = 'name', value = 'AE'},
    {kind = 'name', value = 'Ccedilla'},
    {kind = 'name', value = 'Egrave'},
    {kind = 'name', value = 'Eacute'},
    {kind = 'name', value = 'Ecircumflex'},
    {kind = 'name', value = 'Edieresis'},
    {kind = 'name', value = 'Igrave'},
    {kind = 'name', value = 'Iacute'},
    {kind = 'name', value = 'Icircumflex'},
    {kind = 'name', value = 'Idieresis'},
    {kind = 'name', value = 'Eth'},
    {kind = 'name', value = 'Ntilde'},
    {kind = 'name', value = 'Ograve'},
    {kind = 'name', value = 'Oacute'},
    {kind = 'name', value = 'Ocircumflex'},
    {kind = 'name', value = 'Otilde'},
    {kind = 'name', value = 'Odieresis'},
    {kind = 'name', value = 'multiply'},
    {kind = 'name', value = 'Oslash'},
    {kind = 'name', value = 'Ugrave'},
    {kind = 'name', value = 'Uacute'},
    {kind = 'name', value = 'Ucircumflex'},
    {kind = 'name', value = 'Udieresis'},
    {kind = 'name', value = 'Yacute'},
    {kind = 'name', value = 'Thorn'},
    {kind = 'name', value = 'germandbls'},
    {kind = 'name', value = 'agrave'},
    {kind = 'name', value = 'aacute'},
    {kind = 'name', value = 'acircumflex'},
    {kind = 'name', value = 'atilde'},
    {kind = 'name', value = 'adieresis'},
    {kind = 'name', value = 'aring'},
    {kind = 'name', value = 'ae'},
    {kind = 'name', value = 'ccedilla'},
    {kind = 'name', value = 'egrave'},
    {kind = 'name', value = 'eacute'},
    {kind = 'name', value = 'ecircumflex'},
    {kind = 'name', value = 'edieresis'},
    {kind = 'name', value = 'igrave'},
    {kind = 'name', value = 'iacute'},
    {kind = 'name', value = 'icircumflex'},
    {kind = 'name', value = 'idieresis'},
    {kind = 'name', value = 'eth'},
    {kind = 'name', value = 'ntilde'},
    {kind = 'name', value = 'ograve'},
    {kind = 'name', value = 'oacute'},
    {kind = 'name', value = 'ocircumflex'},
    {kind = 'name', value = 'otilde'},
    {kind = 'name', value = 'odieresis'},
    {kind = 'name', value = 'divide'},
    {kind = 'name', value = 'oslash'},
    {kind = 'name', value = 'ugrave'},
    {kind = 'name', value = 'uacute'},
    {kind = 'name', value = 'ucircumflex'},
    {kind = 'name', value = 'udieresis'},
    {kind = 'name', value = 'yacute'},
    {kind = 'name', value = 'thorn'},
    {kind = 'name', value = 'ydieresis'},
  }}
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
      elseif kind == 'string' then
        return execute_ps(assert(parse_ps(vtok.value), 'syntaxerror'))
      else
        error'Unimplemented'
      end
    elseif ttok == 'number' then
      return push(tok)
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

local ps_tokens
local fid = font.define{
  name = 'dummy virtual font for PS rendering',
  -- type = 'virtual',
  characters = {
    [0x1F3A8] = {
      commands = {
        {'lua', function(fid)
          local n = node.new('glyph', 256)
          n.font = fid
          n.char = 1
          local x, y = pdf.getpos()
          n.xoffset = -x
          n.yoffset = -y
          ps_tokens[1], ps_tokens[2] = x/65781.76, y/65781.76
          n = node.hpack(n)
          vf.node(node.direct.todirect(n))
          node.free(n)
        end}
      }
    },
    [1] = {
      commands = {
        {'lua', function()
          local tokens = ps_tokens
          ps_tokens = nil
          execute_ps(tokens)
          flush_delayed()
        end}
      }
    },
  },
}

local modes = tex.getmodevalues()
local func = luatexbase.new_luafunction'luaPST'
token.set_lua('luaPST', func, 'protected')
lua.get_functions_table()[func] = function()
  local mode = token.scan_keyword'direct' and 'page' or 'origin'
  local tokens = parse_ps(token.scan_argument(true))
  table.move(tokens, 1, #tokens, mode == 'origin' and 5 or 4)
  tokens[1], tokens[2] = 'null', 'null'
  if mode == 'origin' then
    tokens[3], tokens[4] = 'gsave', 'translate'
    tokens[#tokens+1] = 'grestore'
  else
    tokens[3] = 'moveto'
  end
  local n = node.new('whatsit', 'late_lua')
  function n.data()
    local x, y = pdf.getpos()
    tokens[1], tokens[2] = x/65781.76, y/65781.76
    assert(not ps_tokens)
    ps_tokens = tokens
  end
  local nn = node.new('glyph')
  nn.subtype = 256
  nn.font, nn.char = fid, 0x1F3A8
  n.next = nn
  if 'horizontal' ~= modes[math.abs(tex.nest.top.mode)] then
    n = node.hpack(n) -- Glyphs can only appear in hmode
  end
  if tex.nest.ptr == 0 then
    -- Main vertical list. Here we might appear before the page starts properly
    -- and should not freeze page specifications. Since we don't have any outer dimensions,
    -- we can ensure this by sneaking our node into the current page list whithout going though
    -- build_page.
    tex.triggerbuildpage() -- First ensure that everything else is contributed properly.
    tex.lists.page_head = node.insert_after(tex.lists.page_head, nil, n)
  else
    node.write(n) -- might be problematic in math
  end
end
-- luatexbase.add_to_callback('pre_shipout_filter', function(n)
--   print(n)
--   return true
-- end, 'WIP')
-- font.current(fid)
