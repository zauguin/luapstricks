---- luapstricks.lua
-- Copyright 2021--2022 Marcel Krüger <tex@2krueger.de>
--
-- This work may be distributed and/or modified under the
-- conditions of the LaTeX Project Public License, either version 1.3
-- of this license or (at your option) any later version.
-- The latest version of this license is in
--   http://www.latex-project.org/lppl.txt
-- and version 1.3 or later is part of all distributions of LaTeX
-- version 2005/12/01 or later.
--
-- This work has the LPPL maintenance status `maintained'.
-- 
-- The Current Maintainer of this work is M. Krüger
--
-- This work consists of the file luapstricks.lua.

if luatexbase then
  luatexbase.provides_module {
    name = 'luapstricks',
    version = 'v0.5',
    date = '2022-04-20',
    description = 'PSTricks backend for LuaLaTeX',
  }
end

local setwhatsitfield = node.setwhatsitfield or node.setfield
local late_lua_sub = node.subtype'late_lua'

local pdfprint = vf.pdf -- Set later to have the right mode
local function gobble() end

local pi = math.pi
local two_pi = 2*pi
local pi2_inv = 2/pi
local pi3_inv = 3/pi

local sin_table = {0, 1, 0, -1}

local l = lpeg

local whitespace = (l.S'\0\t\n\r\f ' + '%' * (1-l.P'\n')^0 * (l.P'\n' + -1))^1

local regular = 1 - l.S'\0\t\n\r\f %()<>[]{}/'

local exitmarker = {}
local lookup

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
        + l.R'07' * l.R'07'^-2 / function(s) return string.char(tonumber(s, 8) % 0x100) end
        + ('\r' * l.P'\n'^-1 + '\n')^-1 * l.Cc''
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
  + imm_name / function(name) return lookup(name) end
  + l.Ct(l.Cg(literal_name, 'value') * l.Cg(l.Cc'name', 'kind'))
  + name
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

local serialize_pdf do
  function serialize_pdf(obj)
    local t = type(obj)
    if t == 'number' then
      return string.format(math.type(obj) == 'float' and '%.5f' or '%i', obj)
    elseif t == 'boolean' then
      return obj and 'true' or 'false'
    elseif t == 'string' then
      return '/' .. obj
    elseif t == 'table' then
      t = obj.kind
      if t == 'name' then
        return '/' .. obj.value
      elseif t == 'string' then
        return '(' .. obj.value .. ')' -- TODO: Escaping
      elseif t == 'dict' then
        local helper = {}
        for k, v in next, obj.value do
          helper[#helper+1] = serialize_pdf(k)
          helper[#helper+1] = serialize_pdf(v)
        end
        return '<<' .. table.concat(helper, ' ') .. '>>'
      elseif t == 'array' then
        local helper = {}
        for i, v in ipairs(obj.value) do
          helper[i] = serialize_pdf(v)
        end
        return '[' .. table.concat(helper, ' ') .. ']'
      else
        error'Unable to serialize object'
      end
    end
    error'Unable to serialize object'
  end
end

local srand, rrand, rand do
  local state
  function srand(s)
    state = s//1
    if state < 1 then
      state = -(state % 0x7ffffffe) + 1
    elseif state > 0x7ffffffe then
      state = 0x7ffffffe
    end
  end
  function rrand()
    return state
  end
  function rand()
    state = (16807 * state) % 0x7fffffff
    -- if state <= 0 then
    --   state = state + 0x7fffffff
    -- end
    return state
  end
  srand(math.random(1, 0x7ffffffe))
end

local maybe_decompress do
  local compressed_pattern = '%!PS\n\z
    currentfile<</Predictor 1' * l.R'05' * '/Columns ' * (l.R'09'^1/tonumber) * '>>/FlateDecode filter cvx exec\n'
    * l.C(l.P(1)^1)

  local stacklimit = 999000

  function maybe_decompress(data)
    local columns, compressed = compressed_pattern:match(data)
    if not columns then return data end

    data = zlib.decompress(compressed)
    local bytes do
      local size = #data
      if size < stacklimit then
        bytes = {data:byte(1, -1)}
      else
        bytes = {}
        local off = 1
        for i = 1, size, stacklimit do
          table.move({data:byte(i, i+stacklimit-1)}, 1, stacklimit, i, bytes)
        end
      end
    end
    local new_data = {}
    local start_row = 1
    local out_row = 1
    while true do
      local control = bytes[start_row]
      if not control then break end
      if control == 0 or (control == 2 and start_row == 1) then
        table.move(bytes, start_row + 1, start_row + columns, out_row, new_data)
      elseif control == 1 then
        local last = bytes[start_row + 1]
        new_data[out_row] = last
        for i = 2, columns do
          last = (bytes[start_row + i] + last) & 0xFF
          new_data[out_row + i - 1] = last
        end
      elseif control == 2 then
        for i = 1, columns do
          new_data[out_row + i - 1] = (bytes[start_row + i] + new_data[out_row - columns - 1 + i]) & 0xFF
        end
      else
        error'Unimplemented'
      end
      start_row = start_row + columns + 1
      out_row = out_row + columns
    end
    local result = ''
    local size = #new_data
    for i = 1, size, stacklimit do
      result = result .. string.char(table.unpack(new_data, i, i + stacklimit > size and size or i + stacklimit - 1))
    end
    return result
  end
end

local font_aliases = {
  -- First add some help to find the TeX Gyre names under the corresponding URW font names
  ['NimbusRoman-Regular'] = 'kpse:texgyretermes-regular.otf',
  ['NimbusRoman-Italic'] = 'kpse:texgyretermes-italic.otf',
  ['NimbusRoman-Bold'] = 'kpse:texgyretermes-bold.otf',
  ['NimbusRoman-BoldItalic'] = 'kpse:texgyretermes-bolditalic.otf',

  ['NimbusSans-Regular'] = 'kpse:texgyreheros-regular.otf',
  ['NimbusSans-Italic'] = 'kpse:texgyreheros-italic.otf',
  ['NimbusSans-Bold'] = 'kpse:texgyreheros-bold.otf',
  ['NimbusSans-BoldItalic'] = 'kpse:texgyreheros-bolditalic.otf',

  ['NimbusSansNarrow-Regular'] = 'kpse:texgyreheroscn-regular.otf',
  ['NimbusSansNarrow-Oblique'] = 'kpse:texgyreheroscn-italic.otf',
  ['NimbusSansNarrow-Bold'] = 'kpse:texgyreheroscn-bold.otf',
  ['NimbusSansNarrow-BoldOblique'] = 'kpse:texgyreheroscn-bolditalic.otf',

  ['NimbusMonoPS-Regular'] = 'kpse:texgyrecursor-regular.otf',
  ['NimbusMonoPS-Italic'] = 'kpse:texgyrecursor-italic.otf',
  ['NimbusMonoPS-Bold'] = 'kpse:texgyrecursor-bold.otf',
  ['NimbusMonoPS-BoldItalic'] = 'kpse:texgyrecursor-bolditalic.otf',

  ['URWBookman-Light'] = 'kpse:texgyrebonum-regular.otf',
  ['URWBookman-LightItalic'] = 'kpse:texgyrebonum-italic.otf',
  ['URWBookman-Demi'] = 'kpse:texgyrebonum-bold.otf',
  ['URWBookman-DemiItalic'] = 'kpse:texgyrebonum-bolditalic.otf',

  ['URWGothic-Book'] = 'kpse:texgyreadventor-regular.otf',
  ['URWGothic-BookOblique'] = 'kpse:texgyreadventor-italic.otf',
  ['URWGothic-Demi'] = 'kpse:texgyreadventor-bold.otf',
  ['URWGothic-DemiOblique'] = 'kpse:texgyreadventor-bolditalic.otf',

  -- These fonts have weird names in their URW variant, so we use the standard font names directly instead.
  ['NewCenturySchlbk-Roman'] = 'kpse:texgyreschola-regular.otf',
  ['NewCenturySchlbk-Italic'] = 'kpse:texgyreschola-italic.otf',
  ['NewCenturySchlbk-Bold'] = 'kpse:texgyreschola-bold.otf',
  ['NewCenturySchlbk-BoldItalic'] = 'kpse:texgyreschola-bolditalic.otf',

  ['Palatino-Roman'] = 'kpse:texgyrepagella-regular.otf',
  ['Palatino-Italic'] = 'kpse:texgyrepagella-italic.otf',
  ['Palatino-Bold'] = 'kpse:texgyrepagella-bold.otf',
  ['Palatino-BoldItalic'] = 'kpse:texgyrepagella-bolditalic.otf',

  ['ZapfChancery-MediumItalic'] = 'kpse:texgyrechorus-mediumitalic.otf',

  -- The two symbol fonts don't have OpenType equivalents in TeX Live
  -- so we use TFM based fonts instead
  ['StandardSymbolsPS'] = 'usyr',
  ['Dingbats'] = 'uzdr',
}
-- Then map the standard 35 font names to the URW names as done by GhostScript
-- (Except for New Century Schoolbook which got mapped directly before.
for psname, remapped in next, {
  ['Times-Roman'] = 'NimbusRoman-Regular',
  ['Times-Italic'] = 'NimbusRoman-Italic',
  ['Times-Bold'] = 'NimbusRoman-Bold',
  ['Times-BoldItalic'] = 'NimbusRoman-BoldItalic',

  ['Helvetica'] = 'NimbusSans-Regular',
  ['Helvetica-Oblique'] = 'NimbusSans-Italic',
  ['Helvetica-Bold'] = 'NimbusSans-Bold',
  ['Helvetica-BoldOblique'] = 'NimbusSans-BoldItalic',

  ['Helvetica-Narrow'] = 'NimbusSansNarrow-Regular',
  ['Helvetica-Narrow-Oblique'] = 'NimbusSansNarrow-Oblique',
  ['Helvetica-Narrow-Bold'] = 'NimbusSansNarrow-Bold',
  ['Helvetica-Narrow-BoldOblique'] = 'NimbusSansNarrow-BoldOblique',

  ['Courier'] = 'NimbusMonoPS-Regular',
  ['Courier-Oblique'] = 'NimbusMonoPS-Italic',
  ['Courier-Bold'] = 'NimbusMonoPS-Bold',
  ['Courier-BoldOblique'] = 'NimbusMonoPS-BoldItalic',

  ['Bookman-Light'] = 'URWBookman-Light',
  ['Bookman-LightItalic'] = 'URWBookman-LightItalic',
  ['Bookman-Demi'] = 'URWBookman-Demi',
  ['Bookman-DemiItalic'] = 'URWBookman-DemiItalic',

  ['AvantGarde-Book'] = 'URWGothic-Book',
  ['AvantGarde-BookOblique'] = 'URWGothic-BookOblique',
  ['AvantGarde-Demi'] = 'URWGothic-Demi',
  ['AvantGarde-DemiOblique'] = 'URWGothic-DemiOblique',

  ['Symbol'] = 'StandardSymbolsPS',
  ['StandardSymL'] = 'StandardSymbolsPS',

  ['ZapfDingbats'] = 'Dingbats',

  -- Some additional names needed for PSTricks
  ['NimbusRomNo9L-Regu'] = 'NimbusRoman-Regular',
  ['NimbusRomNo9L-ReguItal'] = 'NimbusRoman-Italic',
  ['NimbusRomNo9L-Medi'] = 'NimbusRoman-Bold',
  ['NimbusRomNo9L-MediItal'] = 'NimbusRoman-BoldItalic',
  ['NimbusRomNo9L-Bold'] = 'NimbusRoman-Bold',

  ['NimbusSanL-Regu'] = 'NimbusSans-Regular',
  ['NimbusSanL-ReguItal'] = 'NimbusSans-Italic',
  ['NimbusSanL-Bold'] = 'NimbusSans-Bold',
  ['NimbusSanL-BoldItal'] = 'NimbusSans-BoldItalic',

  ['NimbusSanL-BoldCond'] = 'NimbusSansNarrow-Bold',
  ['NimbusSanL-BoldCondItal'] = 'NimbusSansNarrow-BoldOblique',
  ['NimbusSanL-ReguCond'] = 'NimbusSansNarrow-Regular',
  ['NimbusSanL-ReguCondItal'] = 'NimbusSansNarrow-Oblique',

  ['NimbusMonL-Regu'] = 'NimbusMonoPS-Regular',
  ['NimbusMonL-ReguObli'] = 'NimbusMonoPS-Italic',
  ['NimbusMonL-Bold'] = 'NimbusMonoPS-Bold',
  ['NimbusMonL-BoldObli'] = 'NimbusMonoPS-BoldItalic',

  ['URWBookmanL-DemiBoldItal'] = 'URWBookman-DemiItalic',
  ['URWBookmanL-DemiBold'] = 'URWBookman-Demi',
  ['URWBookmanL-LighItal'] = 'URWBookman-LightItalic',
  ['URWBookmanL-Ligh'] = 'URWBookman-Light',

  ['URWGothicL-BookObli'] = 'URWGothic-BookOblique',
  ['URWGothicL-Book'] = 'URWGothic-Book',
  ['URWGothicL-DemiObli'] = 'URWGothic-DemiOblique',
  ['URWGothicL-Demi'] = 'URWGothic-Demi',

  ['CenturySchL-Roma'] = 'NewCenturySchlbk-Roman',
  ['CenturySchL-Ital'] = 'NewCenturySchlbk-Italic',
  ['CenturySchL-Bold'] = 'NewCenturySchlbk-Bold',
  ['CenturySchL-BoldItal'] = 'NewCenturySchlbk-BoldItalic',

  ['URWPalladioL-Roma'] = 'Palatino-Roman',
  ['URWPalladioL-Ital'] = 'Palatino-Italic',
  ['URWPalladioL-Bold'] = 'Palatino-Bold',
  ['URWPalladioL-BoldItal'] = 'Palatino-BoldItalic',

  ['URWChanceryL-MediItal'] = 'ZapfChancery-MediumItalic',
} do
  font_aliases[psname] = font_aliases[remapped] or remapped
end

local operand_stack = {}

local pushs do
  local function helper(height, args, arg, ...)
    if args == 0 then return end
    height = height + 1
    operand_stack[height] = arg
    return helper(height, args - 1, ...)
  end
  function pushs(...)
    return helper(#operand_stack, select('#', ...), ...)
  end
end
local function push(value)
  operand_stack[#operand_stack+1] = value
end

local function ps_error(kind, ...)
  pushs(...)
  return error{pserror = kind, trace = debug.traceback()}
end

local function pop(...)
  local height = #operand_stack
  if height == 0 then
    return ps_error('stackunderflow', ...)
  end
  local v = operand_stack[height]
  operand_stack[height] = nil
  return v, v
end
local function pop_num(...)
  local raw = pop(...)
  local n = raw
  local tn = type(n)
  if tn == 'table' and n.kind == 'executable' then
    n = n.value
    tn = type(n)
  end
  if tn ~= 'number' then
    ps_error('typecheck', raw, ...)
  end
  return n, raw
end
local pop_int = pop_num
local function pop_proc(...)
  local v = pop()
  if type(v) ~= 'table' or v.kind ~= 'executable' or type(v.value) ~= 'table' or v.value.kind ~= 'array' then
    ps_error('typecheck', v, ...)
  end
  return v.value.value, v
end
local pop_bool = pop
local function pop_dict()
  local orig = pop()
  local dict = orig
  if type(dict) ~= 'table' then
    ps_error('typecheck', orig)
  end
  if dict.kind == 'executable' then
    dict = dict.value
    if type(dict) ~= 'table' then
      ps_error('typecheck', orig)
    end
  end
  if dict.kind ~= 'dict' then
    ps_error('typecheck', orig)
  end
  return dict.value, orig, dict
end
local function pop_array()
  local orig = pop()
  local arr = orig
  if type(arr) == 'table' and arr.kind == 'executable' then
    arr = arr.value
  end
  if type(arr) ~= 'table' or arr.kind ~= 'array' then
    ps_error('typecheck', orig)
  end
  return arr
end
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

-- About the bbox entry:
--   - If the bounding box is not currently tracked, it is set to nil
--   - Otherwise it's a linked list linked with the .next field. Every entry is a "matrix level"
--   - if .bbox[1] is nil, the current matrix level does not have a set bounding box yet
--   - Otherside it's {min_x, min_y, max_x, max_y}
--   - If a .bbox.matrix entry is present then it describes the matrix which should be applied before the bbox gets added to the next "matrix level"
local graphics_stack = {{
  matrix = {10, 0, 0, 10, 0, 0}, -- Chosen for consistency with GhostScript's pdfwrite. Must be the same as defaultmatrix
  bbox = nil,
  linewidth = nil,
  current_path = nil,
  current_point = nil,
  color = {},
  fillconstantalpha = 1,
  strokeconstantalpha = 1,
  alphaisshape = nil,
  blendmode = nil,
  linejoin = nil,
  linecap = nil,
  strokeadjust = nil,
  font = nil,
  dash = nil,
  saved_delayed = nil, -- nil if the `gsave` of this graphic state is not delayed
  flatness = 1,
  miterlimit = nil,
}}

local lua_node_lookup = setmetatable({}, {__mode = 'k'})
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

local write_shading do
  local ShadingCount = 0
  if pdfdict_gput then
    function write_shading(attr, data)
      local obj = pdf.obj{
        type = 'stream',
        immediate = false,
        attr = attr,
        string = data,
      }
      pdf.refobj(obj)
      ShadingCount = ShadingCount + 1
      local name = 'PSShad' .. ShadingCount
      local k = obj .. ' 0 R'
      tex.runtoks(function()
        tex.write(pdfdict_gput, lbrace, 'g__pdf_Core/Page/Resources/Shading', rbrace, lbrace, name, rbrace, lbrace, k, rbrace)
      end)
      ltx.__pdf.Page.Resources.Shading = true
      ltx.pdf.Page_Resources_gpush(tex.count.g_shipout_readonly_int)
      name = '/' .. name
      return name
    end
  else
    function write_shading()
      texio.write_nl"Extended graphic state modifications dropped since `pdfmanagement-testphase' is not loaded."
      return ''
    end
  end
end

local function matrix_transform(x, y, xx, xy, yx, yy, dx, dy)
  return x * xx + y * yx + dx, x * xy + y * yy + dy
end
local function matrix_invert(xx, xy, yx, yy, dx, dy)
  local determinante = xx*yy - xy*yx
  xx, xy, yx, yy = yy/determinante, -xy/determinante, -yx/determinante, xx/determinante
  dx, dy = - dx * xx - dy * yx, - dx * xy - dy * yy
  return xx, xy, yx, yy, dx, dy
end
local delayed = {
  text = {},
  matrix = {1, 0, 0, 1, 0, 0},
}
local function update_matrix(xx, xy, yx, yy, dx, dy)
  local matrix = graphics_stack[#graphics_stack].matrix
  matrix[1], matrix[2],
  matrix[3], matrix[4],
  matrix[5], matrix[6]
    = xx * matrix[1] + xy * matrix[3],             xx * matrix[2] + xy * matrix[4],
      yx * matrix[1] + yy * matrix[3],             yx * matrix[2] + yy * matrix[4],
      dx * matrix[1] + dy * matrix[3] + matrix[5], dx * matrix[2] + dy * matrix[4] + matrix[6]

  local delayed_matrix = delayed.matrix
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
  local delayed_text = delayed.text
  delayed_text[#delayed_text + 1] = str
end

local function reset_delayed(delayed)
  local delayed_matrix = delayed.matrix
  local delayed_text = delayed.text
  for i=1, #delayed_text do
    delayed_text[i] = nil
  end
  delayed_matrix[1], delayed_matrix[2],
  delayed_matrix[3], delayed_matrix[4],
  delayed_matrix[5], delayed_matrix[6] = 1, 0, 0, 1, 0, 0
end

local function flush_delayed_table(delayed, state, force_start)
  local delayed_matrix = delayed.matrix
  local delayed_text = delayed.text

  local cm_string = string.format('%.5f %.5f %.5f %.5f %.5f %.5f cm', delayed_matrix[1], delayed_matrix[2],
                                                                      delayed_matrix[3], delayed_matrix[4],
                                                                      delayed_matrix[5], delayed_matrix[6])
  if cm_string == "1.00000 0.00000 0.00000 1.00000 0.00000 0.00000 cm" then
    cm_string = nil
  else
    local bbox = state.bbox
    if bbox then
      state.bbox = { matrix = delayed_matrix, next = bbox }
      delayed.matrix = {} -- Will be initialized in reset_delayed
    end
  end

  -- Before flushing, make sure that the current graphics state has started.
  graphics_stack_height = graphics_stack_height or #graphics_stack
  local saved_delayed = state.saved_delayed
  if saved_delayed and(cm_string or delayed_text[1] or force_start) then 
    state.saved_delayed = nil
    pdfprint'q'
  end
  for i=1, #delayed_text do
    pdfprint(delayed_text[i])
  end
  if cm_string then
    pdfprint((cm_string:gsub('%.?0+ ', ' ')))
  end
  return reset_delayed(delayed)
end

local function flush_delayed(force_start)
  local pre_first_delayed_group
  for i = #graphics_stack, 1, -1 do
    if not graphics_stack[i].saved_delayed then
      pre_first_delayed_group = i
      break
    end
  end
  for i = pre_first_delayed_group, #graphics_stack-1 do
    flush_delayed_table(graphics_stack[i+1].saved_delayed, graphics_stack[i]) -- No need for force_start here
  end
  return flush_delayed_table(delayed, graphics_stack[#graphics_stack], force_start)
end

local function register_point_bbox(bbox, x, y)
  local min_x, min_y, max_x, max_y = bbox[1], bbox[2], bbox[3], bbox[4]
  if min_x then
    if x < min_x then
      bbox[1] = x
    elseif x > max_x then
      bbox[3] = x
    end
    if y < min_y then
      bbox[2] = y
    elseif y > max_y then
      bbox[4] = y
    end
  else
    bbox[1], bbox[2], bbox[3], bbox[4] = x, y, x, y
  end
end

-- Only call after flush_delayed
local function register_point(state, x, y)
  local bbox = state.bbox
  if not bbox then return end
  return register_point_bbox(bbox, x, y)
end

local function merge_bbox(bbox, after)
  if bbox[1] then
    local matrix = bbox.matrix
    if matrix then
      register_point_bbox(after, matrix_transform(bbox[1], bbox[2], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], matrix[6]))
      register_point_bbox(after, matrix_transform(bbox[1], bbox[4], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], matrix[6]))
      register_point_bbox(after, matrix_transform(bbox[3], bbox[2], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], matrix[6]))
      register_point_bbox(after, matrix_transform(bbox[3], bbox[4], matrix[1], matrix[2], matrix[3], matrix[4], matrix[5], matrix[6]))
    else
      register_point_bbox(after, bbox[1], bbox[2])
      register_point_bbox(after, bbox[3], bbox[4])
    end
  end
  return after
end

function drawarc(xc, yc, r, a1, a2)
  a1, a2 = math.rad(a1), math.rad(a2)
  local dx, dy = r*math.cos(a1), r*math.sin(a1)
  local x, y = xc + dx, yc + dy
  local segments = math.ceil(math.abs(a2-a1)*pi2_inv)
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
function lookup(name)
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

local function ps_to_string(a)
  local ta = type(a)
  if ta == 'table' and a.kind == 'executable' then
    a = a.value
    ta = type(a)
  end
  if ta == 'string' then
  elseif ta == 'boolean' then
    a = a and 'true' or 'false'
  elseif ta == 'number' then
    a = string.format(math.type(a) == 'float' and '%.6g' or '%i', a)
    -- a = tostring(a)
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
  elseif ta == 'userdata' and a.read then
    a = 'file'
  else
    assert(false)
  end
  return a
end

local mark = {kind = 'mark'}
local null = {kind = 'null'}
local statusdict = {kind = 'dict', value = {}}
local globaldict = {kind = 'dict', value = {}}
local userdict = {kind = 'dict', value = {
  SDict = {kind = 'dict', value = {
    normalscale = {kind = 'executable', value = {kind = 'array', value = {}}},
  }},
  TeXDict = {kind = 'dict', value = {
    Resolution = function() push((pdf.getpkresolution())) end,
  }},
  ['@beginspecial'] = {kind = 'executable', value = {kind = 'array', value = {}}},
  ['@setspecial'] = {kind = 'executable', value = {kind = 'array', value = {}}},
  ['@endspecial'] = {kind = 'executable', value = {kind = 'array', value = {}}},
}}
userdict.value.TeXDict.value.VResolution = userdict.value.TeXDict.value.Resolution
local FontDirectory = {kind = 'dict', value = {}}
local ResourceCategories = {kind = 'dict', value = {}}

local function num_to_base(num, base, ...)
  if num == 0 then return string.char(...) end
  local remaining = num // base
  local digit = num - base * remaining
  if digit < 10 then
    digit = digit + 0x30
  else
    digit = digit + 0x37
  end
  return num_to_base(remaining, base, digit, ...)
end

local systemdict
local function generic_show(str, ax, ay)
  local state = graphics_stack[#graphics_stack]
  local current_point = state.current_point
  if not current_point then return nil, 'nocurrentpoint' end
  local rawpsfont = state.font
  if not rawpsfont then return nil, 'invalidfont' end
  local str = str.value
  local psfont = rawpsfont.value
  local fid = psfont.FID
  local matrix = psfont.FontMatrix.value
  local fonttype = psfont.FontType
  if fonttype ~= 0x1CA and fonttype ~= 3 then
    texio.write_nl'luapstricks: Attempt to use unsupported font type.'
    return nil, 'invalidfont'
  end
  local x0, y0 = current_point[1], current_point[2]
  update_matrix(
    matrix[1],      matrix[2],
    matrix[3],      matrix[4],
    matrix[5] + x0, matrix[6] + y0)
  local w = 0
  if fonttype == 0x1CA then
    local characters = assert(font.getfont(fid)).characters
    local max_d, max_h = 0, 0
    flush_delayed()
    if pdfprint ~= gobble then
      vf.push()
      vf.fontid(fid)
    end
    for b in string.bytes(str) do
      if pdfprint ~= gobble then
        vf.char(b)
        if ax then
          vf.right(ax)
          vf.down(-ay)
        end
      end
      local char = characters[b]
      if char then
        w = w + (char.width or 0)
        if char.depth and char.depth > max_d then
          max_d = char.depth
        end
        if char.height and char.height > max_h then
          max_h = char.height
        end
      end
    end
    w = w/65781.76
    if pdfprint ~= gobble then
      max_d = max_d/65781.76
      max_h = max_h/65781.76
      register_point(state, 0, -max_d)
      if ax then
        local count = #str
        register_point(state, w + count * ax, max_h + count * ay)
      else
        register_point(state, w, max_h)
      end
      vf.pop()
    end
  elseif fonttype == 3 then
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
      if ax then
        update_matrix(1, 0, 0, 1, ax, ay)
      end
    end
    update_matrix(1, 0, 0, 1, -w, 0)
    if ax then
      local count = #str
      update_matrix(1, 0, 0, 1, count * -ax, count * -ay)
    end
  else
    assert(false)
  end
  if ax then
    local count = #str
    push(w + count * ax)
    push(count * ay)
  else
    push(w)
    push(0)
  end
  systemdict.value.rmoveto()
  update_matrix(matrix_invert(
    matrix[1],      matrix[2],
    matrix[3],      matrix[4],
    matrix[5] + x0, matrix[6] + y0))
  return true
end

systemdict = {kind = 'dict', value = {
  dup = function()
    local v = pop()
    push(v)
    push(v)
  end,
  exch = function()
    local b = pop()
    local a = pop(b)
    push(b)
    push(a)
  end,
  pop = function()
    pop()
  end,
  clear = function()
    for i = 1, #operand_stack do
      operand_stack[i] = nil
    end
  end,
  copy = function()
    local arg, orig = pop()
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
          ps_error'rangecheck'
        end
        arg.value = src
      elseif kind == 'dict' then
        local src = pop_dict()
        if next(arg.value) then
          error'Target dictionary must be empty'
        end
        for k, v in next, src do
          arg.value[k] = v
        end
      else
        ps_error'typecheck'
      end
      push(exec and {kind = 'executable', value = arg} or arg)
    else
      ps_error('typecheck', orig)
    end
  end,
  roll = function()
    local j, arg2 = pop_int()
    local n, arg1 = pop_int(arg2)
    if n < 0 then
      ps_error('rangecheck', arg1, arg2)
    end
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
    local i, arg1 = pop_int()
    local height = #operand_stack
    if i < 0 or height <= i then
      ps_error('rangecheck', arg1)
    end
    push(operand_stack[height - i])
  end,
  null = function()
    push(null)
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
  ['>>'] = function()
    local mark_pos
    for i = #operand_stack, 1, -1 do
      if operand_stack[i] == mark then
        mark_pos = i
        break
      end
    end
    if not mark_pos then error'unmatchedmark' end
    local dict = lua.newtable(0, (#operand_stack-mark_pos) // 2)
    for i = mark_pos + 1, #operand_stack - 1, 2 do
      push(operand_stack[i])
      local key = pop_key()
      dict[key] = operand_stack[i+1]
    end
    for i = mark_pos, #operand_stack do
      operand_stack[i] = nil
    end
    push{kind = 'dict', value = dict}
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
    error'unmatchedmark'
  end,
  cleartomark = function()
    local entry
    repeat
      entry = pop()
    until (not entry) or type(entry) == 'table' and entry.kind == 'mark'
    if not entry then error'unmatchedmark' end
  end,

  ['if'] = function()
    local proc, arg2 = pop_proc()
    local cond = pop_bool(arg2)
    if cond then
      execute_ps(proc)
    end
  end,
  ifelse = function()
    local proc_else, arg3 = pop_proc()
    local proc_then, arg2 = pop_proc(arg3)
    local cond = pop_bool(arg2, arg3)
    if cond then
      execute_ps(proc_then)
    else
      execute_ps(proc_else)
    end
  end,
  ['for'] = function()
    local proc, arg4 = pop_proc()
    local limit, arg3 = pop_num(arg4)
    local step, arg2 = pop_num(arg3, arg4)
    local initial = pop_num(arg2, arg3, arg4)
    local success, err = pcall(function()
      for i=initial, limit, step do
        push(i)
        execute_ps(proc)
      end
    end)
    if not success and err ~= exitmarker then
      error(err, 0)
    end
  end,
  forall = function()
    local proc, arg2 = pop_proc()
    local obj, arg1 = pop()
    if type(obj) ~= 'table' then
      ps_error('typecheck', arg1, arg2)
    end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then
        ps_error('typecheck', arg1, arg2)
      end
    end
    local success, err = pcall(
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
             pushs(k, v)
             execute_ps(proc)
           end
         end
      or ps_error('typecheck', arg1, arg2))
    if not success and err ~= exitmarker then
      error(err, 0)
    end
  end,
  ['repeat'] = function()
    local proc, arg2 = pop_proc()
    local count = pop_int(arg2)
    local success, err = pcall(function()
      for i=1, count do
        execute_ps(proc)
      end
    end)
    if not success and err ~= exitmarker then
      error(err, 0)
    end
  end,
  loop = function()
    local proc = pop_proc()
    local success, err = pcall(function()
      while true do
        execute_ps(proc)
      end
    end)
    if not success and err ~= exitmarker then
      error(err, 0)
    end
  end,

  reversepath = function()
    local state = graphics_stack[#graphics_stack]
    local path = state.current_path
    if not path then return end
    local newpath = lua.newtable(#path, 0)
    local i = 1
    local out_ptr = 1
    -- Iterate over groups starting with "x y m". These can contain multiple subpaths separated with `h`.
    while path[i+2] == 'm' do
      local x0, y0 = path[i], path[i + 1]
      local after = i + 3
      while path[after] and path[after + 2] ~= 'm' do
        after = after + 1
      end
      local j = after
      out_ptr = out_ptr + 3 -- Leave space for the initial `x y m`
      newpath[out_ptr - 1] = 'm'
      local drop_closepath = true -- If this is true we do not end with a closepath and therefore have to remove the first one.
      while true do
        j = j - 1
        local cmd = path[j]
        if cmd == 'h' then
          if j ~= after - 1 then
            newpath[out_ptr - 3], newpath[out_ptr - 2] = x0, y0
            if not drop_closepath then
              newpath[out_ptr] = 'h'
              out_ptr = out_ptr + 1
            end
            out_ptr = out_ptr + 3 -- Leave space for the initial `x y m`
            newpath[out_ptr - 1] = 'm'
          end
          drop_closepath = false
        elseif cmd == 'm' then
          newpath[out_ptr - 3], newpath[out_ptr - 2] = path[j - 2], path[j - 1]
          break
        else
          if cmd == 'c' then
            newpath[out_ptr - 3], newpath[out_ptr - 2] = path[j - 2], path[j - 1]
            newpath[out_ptr], newpath[out_ptr + 1], newpath[out_ptr + 2], newpath[out_ptr + 3] = path[j - 4], path[j - 3], path[j - 6], path[j - 5]
            out_ptr = out_ptr + 6
            newpath[out_ptr] = 'c'
            j = j - 6
          elseif cmd == 'l' then
            newpath[out_ptr - 3], newpath[out_ptr - 2] = path[j - 2], path[j - 1]
            out_ptr = out_ptr + 2
            j = j - 2
          else
            assert(false)
          end
          newpath[out_ptr] = cmd
          out_ptr = out_ptr + 1
        end
      end
      if not drop_closepath then
        newpath[out_ptr] = 'h'
        out_ptr = out_ptr + 1
      end
      i = after
    end
    state.current_path = newpath
    local last_cmd = #newpath
    if newpath[last_cmd] == 'h' then
      last_cmd = last_cmd - 1
    end
    state.current_point[1], state.current_point[2] = newpath[last_cmd - 2], newpath[last_cmd - 1]
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
    local success, err = pcall( function()
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
    if not success and err ~= exitmarker then
      error(err, 0)
    end
  end,

  ['.texboxforall'] = function()
    local proc, arg2 = pop_proc()
    local boxop = pop()
    local box = lua_node_lookup[boxop]
    if not box then
      -- push(boxop)
      -- -- push(proc)
      ps_error('typecheck', boxop, arg2)
    end
    if node.direct.getid(box.box) ~= node.id'hlist' then
      -- push(boxop)
      -- push(proc)
      error'.texboxforall is currently only supported for hboxes'
    end

    local head = node.direct.getlist(box.box)
    head = node.direct.flatten_discretionaries(head)
    node.direct.setlist(box.box, head)
    local success, err = pcall(function()
      local x, y = 0, 0
      local n = head
      while n do
        local after = node.direct.getnext(n)
        local width = node.direct.rangedimensions(box.box, n, after)/65781.76
        push(mark)
        local id = node.type(node.direct.getid(n))
        local subbox = {box = n, parent = box} -- parent is needed for lifetime reasons
        local function op()
          flush_delayed()
          vf.push()
          local n = subbox.box -- Same as the outer box, but this preserves the lifetime of subbox
          local parent = subbox.parent.box
          local after = node.direct.getnext(n)
          local head = node.direct.getlist(parent)
          node.direct.setnext(n, nil)
          node.direct.setlist(parent, n)
          local state = graphics_stack[#graphics_stack]
          local w, h, d = node.direct.dimensions(n)
          register_point(state, 0, -d/65781.76)
          register_point(state, w/65781.76, h/65781.76)
          vf.node(parent)
          node.direct.setnext(n, after)
          node.direct.setlist(parent, head)
          vf.pop()
        end
        lua_node_lookup[subbox] = op
        push(op)
        push(x)
        push(y)
        push(width)
        push(0)
        push(id)
        execute_ps(proc)
        if width ~= 0 then
          x = x + width
        end
        n = after
      end
    end)
    if not success and err ~= exitmarker then
      error(err, 0)
    end
  end,
  pathbbox = function()
    local current_path = assert(graphics_stack[#graphics_stack].current_path, 'nocurrentpoint')
    local i=1
    local llx, lly, urx, ury
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
    local val, orig = pop()
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
      ps_error('typecheck', orig)
    end
  end,
  ['and'] = function()
    local val, orig = pop()
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
      ps_error('typecheck', orig)
    end
  end,
  ['or'] = function()
    local val, orig = pop()
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
      ps_error('typecheck', orig)
    end
  end,
  ['xor'] = function()
    local val, orig = pop()
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
      ps_error('typecheck', orig)
    end
  end,
  bitshift = function()
    local shift, arg2 = pop_num()
    local val = pop_num(arg2)
    push(val << shift)
  end,

  eq = function()
    local b = pop()
    local a = pop(b)
    if type(a) == 'table' and (a.kind == 'executable' or a.kind == 'name' or a.kind == 'operator') then
      a = a.value
    end
    if type(a) == 'table' and a.kind == 'string' then
      a = a.value
    end
    if type(b) == 'table' and (b.kind == 'executable' or b.kind == 'name' or b.kind == 'operator') then
      b = b.value
    end
    if type(b) == 'table' and b.kind == 'string' then
      b = b.value
    end
    push(a==b)
  end,
  ne = function()
    local b = pop()
    local a = pop(b)
    if type(a) == 'table' and (a.kind == 'executable' or a.kind == 'name' or a.kind == 'operator') then
      a = a.value
    end
    if type(a) == 'table' and a.kind == 'string' then
      a = a.value
    end
    if type(b) == 'table' and (b.kind == 'executable' or b.kind == 'name' or b.kind == 'operator') then
      b = b.value
    end
    if type(b) == 'table' and b.kind == 'string' then
      b = b.value
    end
    push(a~=b)
  end,
  gt = function()
    local b, arg2 = pop()
    local a, arg1 = pop(arg2)
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        ps_error('typecheck', arg1, arg2)
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        ps_error('typecheck', arg1, arg2)
      end
      a, b = a.value, b.value
    else
      ps_error('typecheck', arg1, arg2)
    end
    push(a>b)
  end,
  ge = function()
    local b, arg2 = pop()
    local a, arg1 = pop(arg2)
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        ps_error('typecheck', arg1, arg2)
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        ps_error('typecheck', arg1, arg2)
      end
      a, b = a.value, b.value
    else
      ps_error('typecheck', arg1, arg2)
    end
    push(a>=b)
  end,
  le = function()
    local b, arg2 = pop()
    local a, arg1 = pop(arg2)
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        ps_error('typecheck', arg1, arg2)
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        ps_error('typecheck', arg1, arg2)
      end
      a, b = a.value, b.value
    else
      ps_error('typecheck', arg1, arg2)
    end
    push(a<=b)
  end,
  lt = function()
    local b, arg2 = pop()
    local a, arg1 = pop(arg2)
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        ps_error('typecheck', arg1, arg2)
      end
    elseif ta == 'table' and a.kind == 'string' then
      if tb ~= 'table' or b.kind ~= 'string' then
        ps_error('typecheck', arg1, arg2)
      end
      a, b = a.value, b.value
    else
      ps_error('typecheck', arg1, arg2)
    end
    push(a<b)
  end,

  -- The following two are GhostScript extensions
  max = function()
    local b, arg2 = pop()
    local a, arg1 = pop(arg2)
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        ps_error('typecheck', arg1, arg2)
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        ps_error('typecheck', arg1, arg2)
      end
      a, b = a.value, b.value
    else
      ps_error('typecheck', arg1, arg2)
    end
    push(a > b and a or b)
  end,
  min = function()
    local b, arg2 = pop()
    local a, arg1 = pop(arg2)
    local ta, tb = type(a), type(b)
    if ta == 'table' and a.kind == 'executable' then
      a = a.value ta = type(a)
    end
    if tb == 'table' and b.kind == 'executable' then
      b = b.value tb = type(b)
    end
    if ta == 'number' then
      if tb ~= 'number' then
        ps_error('typecheck', arg1, arg2)
      end
    elseif ta == 'table' and ta.kind == 'string' then
      if tb ~= 'table' or tb.kind ~= 'string' then
        ps_error('typecheck', arg1, arg2)
      end
      a, b = a.value, b.value
    else
      ps_error('typecheck', arg1, arg2)
    end
    push(a < b and a or b)
  end,

  add = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a+b)
  end,
  sub = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a-b)
  end,
  mul = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a*b)
  end,
  div = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a/b)
  end,
  idiv = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a//b)
  end,
  mod = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a%b)
  end,
  exp = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    push(a^b)
  end,
  sqrt = function()
    push(math.sqrt(pop_num()))
  end,
  sin = function()
    local x = pop_num()
    local i, f = math.modf(x/90)
    if f == 0 then
      push(sin_table[i % 4 + 1])
    else
      push(math.sin(math.rad(x)))
    end
  end,
  cos = function()
    local x = pop_num()
    local i, f = math.modf(x/90)
    if f == 0 then
      push(sin_table[(i+1) % 4 + 1])
    else
      push(math.cos(math.rad(x)))
    end
  end,
  atan = function()
    local b, arg2 = pop_num()
    local a = pop_num(arg2)
    local res = math.deg(math.atan(a, b))
    if res < 0 then res = res + 360 end
    push(res)
  end,
  arccos = function()
    push(math.deg(math.acos(pop_num())))
  end,
  arcsin = function()
    push(math.deg(math.asin(pop_num())))
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
  ln = function()
    push(math.log((pop_num())))
  end,
  log = function()
    push(math.log(pop_num(), 10))
  end,
  truncate = function()
    push((math.modf(pop_num())))
  end,
  cvn = function()
    local a, raw = pop()
    if type(a) == 'table' and a.kind == 'executable' then
      local val = a.value
      if type(val) ~= 'table' or val.kind ~= 'string' then
        ps_error('typecheck', raw)
      end
      push(val.value)
    end
    if type(a) ~= 'table' or a.kind ~= 'string' then
      ps_error('typecheck', raw)
    end
    return push{kind = 'name', value = a.value}
  end,
  cvi = function()
    local a, raw = pop()
    if type(a) == 'table' and a.kind == 'executable' then
      a = a.value
    end
    if type(a) == 'table' and a.kind == 'string' then
      a = (number * -1):match(a.value)
      if not a then
        ps_error('syntaxerror', raw)
      end
    end
    if type(a) ~= 'number' then ps_error('typecheck', raw) end
    push(a//1)
  end,
  cvr = function()
    local a, raw = pop()
    if type(a) == 'table' and a.kind == 'executable' then
      a = a.value
    end
    if type(a) == 'table' and a.kind == 'string' then
      a = (number * -1):match(a.value)
      if not a then
        ps_error('syntaxerror', raw)
      end
    end
    if type(a) ~= 'number' then ps_error('typecheck', raw) end
    push(a*1.)
  end,
  cvs = function()
    local old_str, arg2 = pop_string()
    local a, arg1 = pop()
    a = ps_to_string(a)
    if #old_str.value < #a then ps_error('rangecheck', arg1, arg2) end
    old_str.value = a .. string.sub(old_str.value, #a+1, -1)
    return push{kind = 'string', value = a}
  end,
  cvrs = function()
    local old_str, arg3 = pop_string()
    local radix, arg2 = pop_num()
    local num, arg1 = pop_num()
    if radix == 10 then
      num = string.format(math.type(num) == 'float' and '%.6g' or '%i', num)
    else
      num = num//1
      if num < 0 and num >= -0x80000000 then
        num = num + 0x100000000
      end
      if num < 0 then
        ps_error('rangecheck', arg1, arg2, arg3)
      end
      num = num == 0 and '0' or num_to_base(num, radix)
    end
    if #old_str.value < #num then ps_error('rangecheck', arg1, arg2, arg3) end
    old_str.value = num .. string.sub(old_str.value, #num+1, -1)
    return push{kind = 'string', value = num}
  end,

  string = function()
    push{kind = 'string', value = string.rep('\0', (pop_int()))}
  end,
  search = function()
    local seek = pop_string()
    local str = pop_string()
    local start, stop = string.find(str.value, seek.value, 1, true)
    if start then
      push(str_view(str, stop + 1, #str.value - stop))
      push(str_view(str, start, stop - start + 1))
      push(str_view(str, 1, start - 1))
      push(true)
    else
      push(str)
      push(false)
    end
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
    local count, arg3 = pop_int()
    local index, arg2 = pop_int()
    local arr, arg1 = pop()
    if type(arr) ~= 'table' then ps_error('typecheck', arg1, arg2, arg3) end
    if arr.kind == 'executable' then
      arr = arr.value
      if type(arr) ~= 'table' then ps_error('typecheck', arg1, arg2, arg3) end
    end
    if arr.kind == 'string' then
      push(str_view(arr, index + 1, count))
    elseif arr.kind == 'array' then
      -- TODO: At least for the array case, we could use metamethods to make get element sharing behavior
      push{kind = 'array', value = table.move(arr.value, index + 1, index + count, 1, {})}
    else
      ps_error('typecheck', arg1, arg2, arg3)
    end
  end,
  putinterval = function()
    local from, arg2 = pop()
    local index, arg1 = pop_int()
    if type(from) ~= 'table' then ps_error('typecheck', arg1, arg2) end
    if from.kind == 'executable' then
      from = from.value
      if type(from) ~= 'table' then ps_error('typecheck', arg1, arg2) end
    end
    if from.kind == 'string' then
      local to = pop_string()
      from = from.value
      to.value = string.sub(to.value, 1, index) .. from .. string.sub(to.value, index + 1 + #from)
    elseif from.kind == 'array' then
      local to = pop_array()
      table.move(from.value, 1, #from.value, index + 1, to.value)
    else
      ps_error('typecheck', arg1, arg2)
    end
  end,

  dict = function()
    local size = pop_int()
    push{kind = 'dict', value = lua.newtable(0, size)}
  end,
  begin = function()
    local _
    _, _, dictionary_stack[#dictionary_stack + 1] = pop_dict()
  end,
  ['end'] = function()
    if #dictionary_stack <= 3 then
      ps_error'dictstackunderflow'
    end
    dictionary_stack[#dictionary_stack] = nil
  end,
  currentdict = function()
    push(dictionary_stack[#dictionary_stack])
  end,
  bind = function()
    local d = pop()
    push(d)
    if type(d) ~= 'table' then ps_error'typecheck' end
    if d.kind == 'executable' then
      d = d.value
      if type(d) ~= 'table' then ps_error'typecheck' end
    end
    if d.kind ~= 'array' then ps_error'typecheck' end
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
    if type(obj) ~= 'table' then ps_error'typecheck' end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then ps_error'typecheck' end
    end
    local val = obj.value
    if obj.kind == 'string' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then ps_error'rangecheck' end
      push(string.byte(val, key+1))
    elseif obj.kind == 'array' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then ps_error'rangecheck' end
      push(val[key+1])
    elseif obj.kind == 'dict' then
      push(key) key = pop_key()
      push(val[key])
    else
      ps_error'typecheck'
    end
  end,
  put = function()
    local value = pop()
    local key = pop()
    local obj = pop()
    if type(obj) ~= 'table' then ps_error'typecheck' end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then ps_error'typecheck' end
    end
    local val = obj.value
    if obj.kind == 'string' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then ps_error'rangecheck' end
      push(value) value = pop_int()
      obj.value = string.sub(val, 1, key) .. string.char(value) .. string.sub(val, key+2, #val)
    elseif obj.kind == 'array' then
      push(key) key = pop_int()
      if key < 0 or key >= #val then ps_error'rangecheck' end
      val[key+1] = value
    elseif obj.kind == 'dict' then
      push(key) key = pop_key()
      val[key] = value
    else
      ps_error'typecheck'
    end
  end,
  undef = function()
    local key = pop_key()
    local dict = pop_dict()
    dict[key] = nil
  end,
  length = function()
    local obj = pop()
    if type(obj) == 'string' then
      return push(#obj)
    elseif type(obj) ~= 'table' then
      ps_error'typecheck'
    end
    if obj.kind == 'executable' then
      obj = obj.value
      if type(obj) ~= 'table' then ps_error'typecheck' end
    end
    local val = obj.value
    if obj.kind == 'string' then
      push(#val)
    elseif obj.kind == 'name' then
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
      ps_error'typecheck'
    end
  end,

  matrix = function()
    push{kind = 'array', value = {1, 0, 0, 1, 0, 0}}
  end,
  defaultmatrix = function()
    local m = pop_array()
    local mm = m.value
    assert(#mm == 6)
    mm[1], mm[2], mm[3], mm[4], mm[5], mm[6] = 10, 0, 0, 10, 0, 0
    push(m)
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
  currentmiterlimit = function()
    push(assert(graphics_stack[#graphics_stack].miterlimit, 'miterlimit has to be set before it is queried'))
  end,
  currentflat = function()
    push(graphics_stack[#graphics_stack].flatness)
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
  setmiterlimit = function()
    local ml = pop_int()
    graphics_stack[#graphics_stack].miterlimit = ml
    delayed_print(string.format('%.3f M', ml))
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
  setflat = function()
    local flatness = pop_num()
    graphics_stack[#graphics_stack].flatness = flatness
    delayed_print(string.format('%.3f i', flatness))
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
      local i = #current_path
      if i ~= 1 and current_path[i] == 'm' then
        current_path[i-2], current_path[i-1] = x, y
      else
        current_path[i+1], current_path[i+2], current_path[i+3] = x, y, 'm'
      end
      local current_point = state.current_point
      current_point[1], current_point[2] = x, y
    else
      state.current_path = {x, y, 'm'}
      state.current_point = {x, y}
    end
  end,
  rmoveto = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local y = pop_num()
    local x = pop_num()
    local current_point = state.current_point
    x, y = current_point[1] + x, current_point[2] + y
    local i = #current_path
    if i ~= 1 and current_path[i] == 'm' then
      current_path[i-2], current_path[i-1] = x, y
    else
      current_path[i+1], current_path[i+2], current_path[i+3] = x, y, 'm'
    end
    current_point[1], current_point[2] = x, y
  end,
  lineto = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local y = pop_num()
    local x = pop_num()
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2] = x, y, 'l'
    local current_point = state.current_point
    current_point[1], current_point[2] = x, y
  end,
  rlineto = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local y = pop_num()
    local x = pop_num()
    local current_point = state.current_point
    x, y = x + current_point[1], y + current_point[2]
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2] = x, y, 'l'
    current_point[1], current_point[2] = x, y
  end,
  curveto = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local y3 = pop_num()
    local x3 = pop_num()
    local y2 = pop_num()
    local x2 = pop_num()
    local y1 = pop_num()
    local x1 = pop_num()
    local i = #current_path + 1
    current_path[i], current_path[i+1], current_path[i+2], current_path[i+3], current_path[i+4], current_path[i+5], current_path[i+6] = x1, y1, x2, y2, x3, y3, 'c'
    local current_point = state.current_point
    current_point[1], current_point[2] = x3, y3
  end,
  rcurveto = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local current_point = state.current_point
    local x0, y0 = current_point[1], current_point[2]
    local y3 = pop_num() + y0
    local x3 = pop_num() + x0
    local y2 = pop_num() + y0
    local x2 = pop_num() + x0
    local y1 = pop_num() + y0
    local x1 = pop_num() + x0
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
    if current_path[#current_path] == 'h' then return end
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
    local state = graphics_stack[#graphics_stack]
    local current_path = assert(state.current_path, 'nocurrentpoint')
    local current_point = state.current_point
    local x0, y0 = current_point[1], current_point[2]

    local r = pop_num()
    local y2 = pop_num()
    local x2 = pop_num()
    local y1 = pop_num()
    local x1 = pop_num()

    local dx1, dy1 = x1 - x0, y1 - y0
    local dx2, dy2 = x2 - x1, y2 - y1

    local a1 = math.atan(dy1, dx1)
    local a2 = math.atan(dy2, dx2)

    if a1 - pi > a2 then
      a1 = a1 - two_pi
    elseif a2 - pi > a1 then
      a2 = a2 - two_pi
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
    drawarc(cx, cy, r, a1*180/pi, a2*180/pi)

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

  eoclip = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    flush_delayed(true)
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        pdfprint(string.format('%.5f', current_path[i]))
      else
        pdfprint(current_path[i])
      end
    end
    pdfprint'W* n'
  end,
  clip = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    flush_delayed(true)
    for i = 1, #current_path do
      if type(current_path[i]) == 'number' then
        pdfprint(string.format('%.5f', current_path[i]))
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
    flush_delayed()
    local x
    for i = 1, #current_path do
      local value = current_path[i]
      if type(value) == 'number' then
        current_path[i] = string.format('%.5f', value)
        if x then
          register_point(state, x, value)
          x = nil
        else
          x = value
        end
      end
    end
    pdfprint((table.concat(current_path, ' '):gsub('%.?0+ ', ' ')))
    state.current_path, state.current_point = nil
  end,
  fill = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    current_path[#current_path+1] = 'f'
    flush_delayed()
    local x
    for i = 1, #current_path do
      local value = current_path[i]
      if type(value) == 'number' then
        current_path[i] = string.format('%.5f', value)
        if x then
          register_point(state, x, value)
          x = nil
        else
          x = value
        end
      end
    end
    pdfprint((table.concat(current_path, ' '):gsub('%.?0+ ', ' ')))
    state.current_path, state.current_point = nil
  end,
  stroke = function()
    local state = graphics_stack[#graphics_stack]
    local current_path = state.current_path
    if not current_path then return end
    current_path[#current_path+1] = 'S'
    flush_delayed()
    local x
    for i = 1, #current_path do
      local value = current_path[i]
      if type(value) == 'number' then
        current_path[i] = string.format('%.5f', value)
        if x then
          register_point(state, x, value)
          x = nil
        else
          x = value
        end
      end
    end
    pdfprint((table.concat(current_path, ' '):gsub('%.?0+ ', ' ')))
    state.current_path, state.current_point = nil
  end,
  flattenpath = function()
    local state = graphics_stack[#graphics_stack]
    local old_path = state.current_path
    if not old_path then return end
    local new_path = {}
    local last_x, last_y = nil, 0
    local saved_x, saved_y
    local subpath_x, subpath_y
    local last_op = 1
    local matrix = state.matrix
    local tolerance = state.flatness / math.sqrt(matrix[1]*matrix[4]-matrix[2]*matrix[3])
    for i=1, #old_path do
      local entry = old_path[i]
      if type(entry) == 'string' then
        if entry == 'c' then
          assert(i - last_op == 6)
          flatten(new_path, tolerance, saved_x, saved_y, table.unpack(old_path, last_op, i-1))
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

  rectclip = function()
    flush_delayed()
    local top = pop()
    if type(top) == 'table' and top.kind == 'executable' then
      top = top.value
    end
    if type(top) == 'number' then
      local h = top
      local w = pop_num()
      local y = pop_num()
      local x = pop_num()
      pdfprint((string.format('%.5f %.5f %.5f %.5f re W n', x, y, w, h):gsub('%.?0+ ', ' ')))
    else
      error'Unsupported rectclip variant'
    end
  end,
  rectstroke = function()
    flush_delayed()
    local top = pop()
    if type(top) == 'table' and top.kind == 'executable' then
      top = top.value
    end
    if type(top) == 'number' then
      local h = top
      local w = pop_num()
      local y = pop_num()
      local x = pop_num()
      pdfprint((string.format('%.5f %.5f %.5f %.5f re S', x, y, w, h):gsub('%.?0+ ', ' ')))
      local state = graphics_stack[#graphics_stack]
      register_point(state, x, y)
      register_point(state, x + w, y + h)
    else
      error'Unsupported rectstroke variant'
    end
  end,
  rectfill = function()
    flush_delayed()
    local top = pop()
    if type(top) == 'table' and top.kind == 'executable' then
      top = top.value
    end
    if type(top) == 'number' then
      local h = top
      local w = pop_num()
      local y = pop_num()
      local x = pop_num()
      pdfprint((string.format('%.5f %.5f %.5f %.5f re f', x, y, w, h):gsub('%.?0+ ', ' ')))
      local state = graphics_stack[#graphics_stack]
      register_point(state, x, y)
      register_point(state, x + w, y + h)
    else
      error'Unsupported rectfill variant'
    end
  end,

  shfill = function()
    local shading_dict, arg1 = pop_dict()
    flush_delayed()
    local data_src
    local pdf_dict = ''
    for k, v in next, shading_dict do
      if k == 'DataSource' then
        data_src = v
      else
        pdf_dict = pdf_dict .. serialize_pdf(k) .. ' ' .. serialize_pdf(v)
      end
    end
    if shading_dict.ShadingType == 4 then
      assert(data_src)
      if type(data_src) ~= 'table' then
        push(arg1)
        ps_error'typecheck'
      end
      if data_src.kind == 'string' then
        data_src = data_src.value
      elseif data_src.kind == 'array' then
        data_src = data_src.value
        local color_model = shading_dict.ColorSpace.value[1]
        if type(color_model) == 'table' and color_model.kind == 'name' then
          color_model = color_model.value
        end
        if color_model == 'DeviceRGB' then
          color_model = 3
        elseif color_model == 'DeviceCMYK' then
          color_model = 4
        elseif color_model == 'DeviceGray' then
          color_model = 1
        else
          error'Unsupported color model in Shading dictionary'
        end
        local components = color_model + 3
        pdf_dict = pdf_dict .. '/BitsPerCoordinate 24/BitsPerComponent 8/BitsPerFlag 8/Decode[-8192 8191 -8192 8191' .. string.rep(' 0 1', color_model) .. ']'
        local data = ''
        for i = 1, #data_src-components+1, components do
          data = data .. string.pack('>BI3I3', data_src[i], (data_src[i+1]*1024+.5)//1 + 8388608, (data_src[i+2]*1024+.5)//1 + 8388608)
          for j = i + 3, i + 2 + color_model do
            data = data .. string.pack('B', (data_src[j]*255+.5)//1)
          end
        end
        data_src = data
      else
        error'Unsupported DataSource variant'
      end
      local obj = write_shading(pdf_dict, data_src)
      pdfprint(string.format('%s sh', write_shading(pdf_dict, data_src)))
    end
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
  invertmatrix = function()
    local target = pop_array()
    local T = target.value
    assert(#T == 6)
    local M = pop_array().value
    assert(#M == 6)
    T[1], T[2], T[3], T[4], T[5], T[6]
      = matrix_invert(M[1], M[2], M[3], M[4], M[5], M[6])
    push(target)
  end,
  concat = function()
    local m = pop_array().value
    if #m ~= 6 then error'Unexpected size of matrix' end
    update_matrix(m[1], m[2], m[3], m[4], m[5], m[6])
  end,
  -- setmatrix is not supported in PDF, so we invert the old matrix first
  setmatrix = function()
    local m = pop()
    if type(m) ~= 'table' or m.kind ~= 'array' then
      ps_error'typecheck'
    end
    local m = m.value
    if #m ~= 6 then ps_error'rangecheck' end
    local old = graphics_stack[#graphics_stack].matrix
    local pt = graphics_stack[#graphics_stack].current_point
    local a, b, c, d, e, f = matrix_invert(old[1], old[2], old[3], old[4], old[5], old[6])
    update_matrix(a, b, c, d, e, f)
    update_matrix(m[1], m[2], m[3], m[4], m[5], m[6])
  end,
  setpdfcolor = function()
    local pdf = pop_string().value
    local color = graphics_stack[#graphics_stack].color
    delayed_print(pdf)
    color.space = {kind = 'array', value = {{kind = 'name', value = 'PDF'}}}
    for i=2, #color do color[i] = nil end
    color[1] = pdf
  end,
  setgray = function()
    local g = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = {kind = 'array', value = {{kind = 'name', value = 'DeviceGray'}}}
    for i=2, #color do color[i] = nil end
    color[1] = g
    delayed_print(string.format('%.3f g %.3f G', g, g))
  end,
  setrgbcolor = function()
    local b = pop_num()
    local g = pop_num()
    local r = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = {kind = 'array', value = {{kind = 'name', value = 'DeviceRGB'}}}
    for i=4, #color do color[i] = nil end
    color[1], color[2], color[3] = r, g, b
    delayed_print(string.format('%.3f %.3f %.3f rg %.3f %.3f %.3f RG', r, g, b, r, g, b))
  end,
  -- Conversion based on Wikipedia article about HSB colorspace
  sethsbcolor = function()
    local b = pop_num()
    local s = pop_num()
    local h = pop_num()
    if b < 0 then b = 0 elseif b > 1 then b = 1 end
    if s < 0 then s = 0 elseif s > 1 then s = 1 end
    if h < 0 then h = 0 elseif h > 1 then h = 1 end
    local hi, hf = math.modf(6 * h)
    local p, q, t = b * (1 - s), b * (1 - s*hf), b * (1 - s * (1-hf))
    if hi == 0 or hi == 6 then
      push(b) push(t) push(p)
    elseif hi == 1 then
      push(q) push(b) push(p)
    elseif hi == 2 then
      push(p) push(b) push(t)
    elseif hi == 3 then
      push(p) push(q) push(b)
    elseif hi == 4 then
      push(t) push(p) push(b)
    elseif hi == 5 then
      push(b) push(p) push(q)
    end
    return systemdict.value.setrgbcolor()
  end,
  setcmykcolor = function()
    local k = pop_num()
    local y = pop_num()
    local m = pop_num()
    local c = pop_num()
    local color = graphics_stack[#graphics_stack].color
    color.space = {kind = 'array', value = {{kind = 'name', value = 'DeviceCMYK'}}}
    for i=5, #color do color[i] = nil end
    color[1], color[2], color[3], color[4] = c, m, y, k
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
  ['.currentalphaisshape'] = function()
    local ais = graphics_stack[#graphics_stack].alphaisshape
    if ais == nil then error'alphaisshape has to be set before it is queried' end
    push(ais)
  end,
  ['.setalphaisshape'] = function()
    local ais = pop_bool()
    graphics_stack[#graphics_stack].alphaisshape = ais
    delayed_print(ExtGState['<</AIS ' .. (ais and 'true' or 'false') .. '>>'])
  end,
  ['.currentblendmode'] = function()
    local blendmode = graphics_stack[#graphics_stack].blendmode
    if blendmode == nil then error'blendmode has to be set before it is queried' end
    push{kind = 'name', value = blendmode}
  end,
  ['.setblendmode'] = function()
    local blendmode = pop()
    if type(blendmode) == 'string' then
    elseif type(blendmode) == 'table' and blendmode.kind == 'name' then
      blendmode = blendmode.value
    else
      push(blendmode)
      ps_error'typecheck'
    end
    graphics_stack[#graphics_stack].blendmode = blendmode
    delayed_print(ExtGState['<</BM /' .. blendmode .. '>>'])
  end,
  newpath = function()
    local state = graphics_stack[#graphics_stack]
    state.current_point = nil
    state.current_path = nil
  end,

  currentcolorspace = function()
    local color = graphics_stack[#graphics_stack].color
    if not color then error'Color has to be set before it is queried' end
    push(color.space)
  end,
  currentcolor = function()
    local color = graphics_stack[#graphics_stack].color
    if not color then error'Color has to be set before it is queried' end
    for i = 1, #color do
      push(color[i])
    end
  end,
  currentcmykcolor = function()
    local c, m, y, k
    local color = graphics_stack[#graphics_stack].color
    if not color then error'Color has to be set before it is queried' end
    local space = color.space.value[1]
    if type(space) == 'table' and space.kind == 'name' then space = space.value end
    if space == 'DeviceRGB' then
      c, m, y = 1 - color[1], 1 - color[2], 1 - color[3]
      -- k = math.min(c, m, y)
      -- TODO: Undercolor removal/black generation
      -- local undercolor = undercolorremoval(k)
      -- local undercolor = 0
      -- k = blackgeneration(k)
      k = 0
      -- c, m, y = c - undercolor, y - undercolor, k - undercolor
    elseif space == 'DeviceGray' then
      c, m, y, k = 0, 0, 0, 1 - color[1]
    elseif space == 'DeviceCMYK' then
      c, m, y, k = color[1], color[2], color[3], color[4]
    elseif space == 'PDF' then
      c, m, y, k = 0, 0, 0, 1
      print('???', 'tocmyk', color[1])
    else
      r, g, b, k = 0, 0, 0, 1
    end
    push(r)
    push(g)
    push(b)
  end,
  currentgraycolor = function()
    local g
    local color = graphics_stack[#graphics_stack].color
    if not color then error'Color has to be set before it is queried' end
    local space = color.space.value[1]
    if type(space) == 'table' and space.kind == 'name' then space = space.value end
    if space == 'DeviceRGB' then
      g = 0.3 * color[1] + 0.59 * color[2], 0.11 * color[3]
    elseif space == 'DeviceGray' then
      g = color[1]
    elseif space == 'DeviceCMYK' then
      g = math.min(1, math.max(0, 0.3 * color[1] + 0.59 * color[2] + 0.11 * color[3] + color[4]))
    elseif space == 'PDF' then
      g = 1
      print('???', 'togray', color[1])
    else
      g = 1
    end
    push(g)
  end,
  currentrgbcolor = function()
    local r, g, b
    local color = graphics_stack[#graphics_stack].color
    if not color then error'Color has to be set before it is queried' end
    local space = color.space.value[1]
    if type(space) == 'table' and space.kind == 'name' then space = space.value end
    if space == 'DeviceRGB' then
      r, g, b = color[1], color[2], color[3]
    elseif space == 'DeviceGray' then
      r = color[1]
      g, b = r, r
    elseif space == 'DeviceCMYK' then
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
  currenthsbcolor = function()
    systemdict.value.currentrgbcolor()
    local b = pop_num()
    local g = pop_num()
    local r = pop_num()
    local M, m = math.max(r, g, b), math.min(r, g, b)
    local H
    if M == m then
      H = 0
    elseif M == r then
      H = (g-b)/(M-m) / 6
      if H < 0 then H = H + 1 end
    elseif M == g then
      H = (b-r)/(M-m) / 6 + 1/3
    elseif assert(M == b) then
      H = (r-g)/(M-m) / 6 + 2/3
    end
    local S = M == 0 and 0 or (M-m)/M
    local B = M
    push(H)
    push(S)
    push(B)
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
    local bbox = graphics_stack[#graphics_stack].bbox
    graphics_stack[#graphics_stack+1] = table.copy(graphics_stack[#graphics_stack], bbox and {[bbox] = {}})
    graphics_stack[#graphics_stack].saved_delayed = delayed
    delayed = {
      text = {},
      matrix = {1, 0, 0, 1, 0, 0},
    }
  end,
  grestore = function()
    local state = graphics_stack[#graphics_stack]
    local saved_delayed = state.saved_delayed
    if saved_delayed then
      delayed = saved_delayed
    else
      pdfprint'Q'
      reset_delayed(delayed)
    end
    local upper_state = graphics_stack[#graphics_stack-1]
    local upper_bbox = upper_state.bbox
    if upper_bbox then
      local bbox = assert(state.bbox)
      while upper_bbox ~= bbox do
        bbox = merge_bbox(bbox, bbox.next or upper_bbox)
      end
    end
    graphics_stack[#graphics_stack] = nil
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
    for i=#operand_stack, 1, -1 do
      texio.write_nl('term and log', ps_to_string(operand_stack[i]))
    end
  end,
  ['='] = function()
    texio.write_nl('term and log', ps_to_string(pop()))
  end,
  ['=='] = function() -- FIXME: Should give a better representation
    texio.write_nl('term and log', ps_to_string(pop()))
  end,

  stringwidth = function()
    local state = graphics_stack[#graphics_stack]
    local rawpsfont = assert(state.font, 'invalidfont')
    local str = pop_string().value
    local psfont = rawpsfont.value
    local fid = psfont.FID
    local matrix = psfont.FontMatrix.value
    local fonttype = psfont.FontType
    if fonttype ~= 0x1CA and fonttype ~= 3 then
      texio.write_nl'luapstricks: Attempt to use unsupported font type.'
      ps_error('invalidfont')
    end
    local w = 0
    if fonttype == 0x1CA then
      local characters = assert(font.getfont(fid)).characters
      for b in string.bytes(str) do
        local char = characters[b]
        w = w + (char and char.width or 0)
      end
      w = w/65781.76
    elseif fonttype == 3 then
      local saved_delayed = delayed
      delayed = {
        text = {},
        matrix = {1, 0, 0, 1, 0, 0},
      }
      local saved_saved_delayed = state.saved_delayed
      state.saved_delayed = nil
      local saved_pdfprint = pdfprint
      pdfprint = gobble
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
      pdfprint = saved_pdfprint
      state.saved_delayed = saved_saved_delayed
      delayed = saved_delayed
    end
    local x, y = matrix_transform(w, 0,
      matrix[1], matrix[2],
      matrix[3], matrix[4],
      0, 0)
    push(x)
    push(y)
  end,
  ashow = function()
    local str, arg3 = pop_string()
    local ay, arg2 = pop_num(arg3)
    local ax, arg1 = pop_num(arg2, arg3)
    local res, err = generic_show(str, ax, ay)
    if not res then
      ps_error(err, arg1, arg2, arg3)
    end
  end,
  show = function()
    local str, orig = pop_string()
    local res, err = generic_show(str)
    if not res then
      ps_error(err, orig)
    end
  end,
  definefont = function()
    local fontdict, raw_fontdict = pop_dict()
    local fontkey = pop_key()
    fontdict.FontMatrix = fontdict.FontMatrix or {kind = 'array', value = {1, 0, 0, 1, 0, 0}}
    if assert(fontdict.FontType) == 0x1CA then
      local fontname = fontdict.FontName
      if type(fontname) == 'table' and fontname.kind == 'name' then
        fontname = fontname.value
      elseif type(fontname) ~= 'string' then
        pushs(fontkey, raw_fontdict)
        ps_error'typecheck'
      end
      local fid = fonts.definers.read(fontname, 65782)
      if not fid then ps_error'invalidfont' end
      if not tonumber(fid) then
        local data = fid
        fid = font.define(data)
        fonts.definers.register(data, fid)
      end
      fontdict.FID = fid
    elseif fontdict.FontType == 3 then
    else
      texio.write_nl'luapstricks: definefont has been called with a font type which is not supported by luapstricks. I will continue, but attempts to use this font will fail.'
    end
    FontDirectory[fontkey] = raw_fontdict
    push(raw_fontdict)
  end,
  makefont = function()
    local m = pop_array().value
    if #m ~= 6 then error'Unexpected size of matrix' end
    local fontdict = pop_dict()
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
    local fontdict = pop_dict()
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
    local _, _, fontdict = pop_dict()
    local state = graphics_stack[#graphics_stack]
    state.font = fontdict
  end,
  ['.findfontid'] = function()
    local fid = pop_int()

    if font.frozen(fid) == nil then
      push(fid)
      ps_error'invalidfont'
    end
    local fontsize_inv = 65782/pdf.getfontsize(fid)
    local fontname = tex.fontname(fid)
    return push{kind = 'dict', value = {
      FID = fid,
      FontMatrix = {kind = 'array', value = {fontsize_inv, 0, 0, fontsize_inv, 0, 0}},
      FontName = {kind = 'name', value = fontname},
      FontType = 0x1CA,
    }}
  end,
  findfont = function()
    local fontname = pop_key()
    local fontdict = FontDirectory[fontname]
    if fontdict then push(fontdict) return end

    fontname = font_aliases[fontname] or fontname
    local fid = fonts.definers.read(fontname, 65782)
    if not fid then ps_error'invalidfont' end
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
  selectfont = function()
    systemdict.value.exch()
    systemdict.value.findfont()
    systemdict.value.exch()
    if type(operand_stack[#operand_stack]) == 'number' then
      systemdict.value.scalefont()
    else
      systemdict.value.makefont()
    end
    systemdict.value.setfont()
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

  findresource = function()
    local category = pop_key()
    local catdict = ResourceCategories.value[category]
    if not catdict then
      push(category)
      print('undefined resource category', category)
      ps_error'undefined'
    end
    local dict_height = #dictionary_stack + 1
    dictionary_stack[dict_height] = catdict
    execute_tok'FindResource'
    if #dictionary_stack ~= dict_height or dictionary_stack[dict_height] ~= catdict then
      error'Messed up dictionary stack in custom resource'
    end
    dictionary_stack[dict_height] = nil
  end,
  resourcestatus = function()
    local category = pop_key()
    local catdict = ResourceCategories.value[category]
    if not catdict then
      push(category)
      print('undefined resource category', category)
      ps_error'undefined'
    end
    local dict_height = #dictionary_stack + 1
    dictionary_stack[dict_height] = catdict
    execute_tok'ResourceStatus'
    if #dictionary_stack ~= dict_height or dictionary_stack[dict_height] ~= catdict then
      error'Messed up dictionary stack in custom resource'
    end
    dictionary_stack[dict_height] = nil
  end,
  resourceforall = function()
    local category = pop_key()
    local catdict = ResourceCategories.value[category]
    if not catdict then
      push(category)
      print('undefined resource category', category)
      ps_error'undefined'
    end
    local dict_height = #dictionary_stack + 1
    dictionary_stack[dict_height] = catdict
    execute_tok'ResourceForAll'
    if #dictionary_stack ~= dict_height or dictionary_stack[dict_height] ~= catdict then
      error'Messed up dictionary stack in custom resource'
    end
    dictionary_stack[dict_height] = nil
  end,
  defineresource = function()
    local category = pop_key()
    local catdict = ResourceCategories.value[category]
    if not catdict then
      push(category)
      print('undefined resource category', category)
      ps_error'undefined'
    end
    local dict_height = #dictionary_stack + 1
    dictionary_stack[dict_height] = catdict
    execute_tok'DefineResource'
    if #dictionary_stack ~= dict_height or dictionary_stack[dict_height] ~= catdict then
      error'Messed up dictionary stack in custom resource'
    end
    dictionary_stack[dict_height] = nil
  end,
  undefineresource = function()
    local category = pop_key()
    local catdict = ResourceCategories.value[category]
    if not catdict then
      push(category)
      print('undefined resource category', category)
      ps_error'undefined'
    end
    local dict_height = #dictionary_stack + 1
    dictionary_stack[dict_height] = catdict
    execute_tok'UndefineResource'
    if #dictionary_stack ~= dict_height or dictionary_stack[dict_height] ~= catdict then
      error'Messed up dictionary stack in custom resource'
    end
    dictionary_stack[dict_height] = nil
  end,

  realtime = function()
    push(os.gettimeofday() * 1000 // 1)
  end,

  rrand = function()
    push(rrand())
  end,
  srand = function()
    srand(pop_int())
  end,
  rand = function()
    push(rand())
  end,

  readonly = function() end, -- Concept not implemented
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
    push(tname)
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
    return execute_tok((pop()))
  end,
  stopped = function()
    local proc = pop()
    local success, err = pcall(execute_tok, proc)
    if success then
      push(false)
    elseif err == 'stop' or true then -- Since we don implement error handlers, all errors act like their error handler included "stop"
      push(true)
    end
  end,
  stop = function()
    error'stop'
  end,
  exit = function()
    error(exitmarker)
  end,
  quit = function()
    os.exit()
  end,
  run = function()
    local filename = pop_string().value
    local resolved = kpse.find_file(filename, 'PostScript header')
    if not resolved then
      error(string.format('Unable to find file %q.', filename))
    end
    local f = assert(io.open(resolved, 'rb'))
    local data = maybe_decompress(f:read'a')
    f:close()
    return execute_tok{kind = 'executable', value = {kind = 'string', value = data}}
  end,

  -- We don't implement local/global separation, so we ignore setglobal and always report currentglobal as true
  setglobal = function()
    pop()
  end,
  currentglobal = function()
    push(true)
  end,

  closefile = function()
    local f = pop()
    f:close()
  end,
  file = function()
    local access = pop_string()
    local orig_filename = pop_string()
    local filename = orig_filename.value
    if access.value:sub(1, 1) == 'a' then
      filename = kpse.find_file(filename)
      if not filename then
        push(orig_filename)
        push(access)
        ps_error'undefinedfilename'
      end
    end
    if access.value == '' then
      push(orig_filename)
      push(access)
      ps_error'invalidfileaccess'
    end
    local f = io.open(filename, access.value)
    if not f then
      push(orig_filename)
      push(access)
      ps_error'invalidfileaccess'
    end
    push(f)
  end,
  write = function()
    local data = pop_num()
    local f = pop()
    data = data % 256
    f:write(string.char(data))
  end,
  writestring = function()
    local data = pop_string().value
    local f = pop()
    f:write(data)
  end,
  readstring = function()
    local target = pop_string()
    local f = pop()
    local data = f:read(#target.value)
    if #target.value == #data then
      target.value = data
      push(target)
      push(true)
      systemdict.value.stack()
    else
      target = str_view(target, 1, #data)
      target.value = data
      push(target)
      push(false)
      systemdict.value.stack()
    end
  end,
  readline = function()
    local target = pop_string()
    local f = pop()
    local data = f:read'L' -- TODO: \r should be accepted as EOL marker too
    if data then
      if #data > #target.value then
        push(f)
        push(target)
        ps_error'rangecheck'
      end
      target = str_view(target, 1, #data)
      target.value = data
      push(target)
      push(true)
    else
      push{kind = 'string', value = ''}
      push(false)
    end
  end,

  token = function()
    local arg = pop()
    if type(arg) ~= 'table' or arg.kind ~= 'string' then
      push(arg)
      if type(arg) == 'userdata' and arg.read then
        error'token applied to file arguments is no yet implemented'
      else
        ps_error'typecheck'
      end
    end
    local str = arg.value
    local tok, after = l.match(any_object * l.Cp(), str)
    if after == nil then
      if l.match(whitespace^-1 * -1, str) then
        push(false)
      else
        push(arg)
        ps_error'syntaxerror'
      end
    else
      push(str_view(arg, after, #str - after + 1))
      push(tok)
      push(true)
    end
  end,

  ['.trackbbox'] = function()
    local state = graphics_stack[#graphics_stack]
    flush_delayed()
    state.bbox = { next = state.bbox, start = true }
  end,
  -- Trackedbbox should only be invoked if the current matrix is essentially the same
  -- as in the corresponding .trackbbox, otherwise everything gets messed up.
  -- This isn't checked, mostly because we don't want a check to be too sensitive.
  ['.trackedbbox'] = function()
    local state = graphics_stack[#graphics_stack]
    local bbox = state.bbox
    if not bbox then
      error'trackedbbox without matching trackbbox'
    end
    while not bbox.start do
      if not bbox.next then
        error'Illegal nesting of trackbbox/trackedbbox and gsave/grestore'
      end
      bbox = merge_bbox(bbox, bbox.next)
    end
    state.bbox = bbox.next and merge_bbox(bbox, bbox.next)
    push(bbox[1] or 0)
    push(bbox[2] or 0)
    push(bbox[3] or 0)
    push(bbox[4] or 0)
  end,

  revision = 1000,
  ['true'] = true,
  ['false'] = false,
  systemdict = systemdict,
  statusdict = statusdict,
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
dictionary_stack = {systemdict, globaldict, userdict, userdict.value.TeXDict}
-- local execution_stack = {} -- Currently not implemented

-- Quite some stuff is missing here since these aren't implemented yet. Anyway mostly useful for testing.
ResourceCategories.value.Font = {kind = 'dict', value = {
  Category = {kind = 'name', value = 'Font'},
  InstanceType = 'dicttype',
  DefineResource = systemdict.value.definefont,
  FindResource = systemdict.value.findfont,
}}

ResourceCategories.value.Generic = {kind = 'dict', value = {
  Category = {kind = 'name', value = 'Generic'},
  DefineResource = function()
    local instance = pop()
    local key = pop_key()
    execute_tok'.Instances'
    local instances = pop_dict()
    instances[key] = instance
    push(instance)
  end,
  UndefineResource = function()
    local key = pop_key()
    execute_tok'.Instances'
    local instances = pop_dict()
    instances[key] = nil
  end,
  FindResource = function()
    local key = pop_key()
    execute_tok'.Instances'
    local instances = pop_dict()
    local instance = instances[key]
    if instance then
      push(instance)
      return
    end
    push(key)
    ps_error'undefinedresource'
  end,
  -- ResourceStatus = function()
  --   local key = pop_key()
  --   execute_tok'.Instances'
  --   local instances = pop_dict()
  --   local instance = instances[key]
  --   if instance then
  --     push(instance)
  --     return
  --   end
  --   push(key)
  --   ps_error'undefinedresource'
  -- end,
  -- ResourceForAll = function()
  --   local key = pop_key()
  --   execute_tok'.Instances'
  --   local instances = pop_dict()
  --   local instance = instances[key]
  --   if instance then
  --     push(instance)
  --     return
  --   end
  --   push(key)
  --   ps_error'undefinedresource'
  -- end,
  ['.Instances'] = {kind = 'dict', value = {}},
}}

local register_texbox do
  local meta = {__gc = function(t) node.direct.free(t.box) end}
  local dict = {}
  ResourceCategories.value['.TeXBox'] = {kind = 'dict', value = {
    Category = {kind = 'name', value = '.TeXBox'},
    DefineResource = function()
      push{kind = 'name', value = '.TeXBox'}
      ps_error'undefined'
    end,
    UndefineResource = function()
      local key = pop_key()
      dict[key] = nil
    end,
    FindResource = function()
      local key = pop_key()
      local instance = dict[key]
      if instance then
        push(instance)
        return
      end
      push(key)
      ps_error'undefinedresource'
    end,
  }}
  local id = 0
  function register_texbox(box)
    id = id + 1
    box = setmetatable({box = node.direct.todirect(box)}, meta)
    local op = function()
      flush_delayed()
      local state = graphics_stack[#graphics_stack]
      local w, h, d = node.direct.dimensions(box.box)
      register_point(state, 0, -d/65781.76)
      register_point(state, w/65781.76, h/65781.76)
      vf.push()
      vf.node(box.box)
      vf.pop()
    end
    lua_node_lookup[op] = box
    dict[id] = op
    return id
  end
end

ResourceCategories.value.Category = {kind = 'dict', value = {
  Category = {kind = 'name', value = 'Generic'},
  InstanceType = 'dicttype',
  DefineResource = function()
    local instance = pop()
    local key = pop_key()
    ResourceCategories.value[key] = instance
    push(instance)
  end,
  UndefineResource = function()
    local key = pop_key()
    ResourceCategories.value[key] = nil
  end,
  FindResource = function()
    local key = pop_key()
    local instance = ResourceCategories.value[key]
    if instance then
      push(instance)
      return
    end
    push(key)
    ps_error'undefinedresource'
  end,
  -- ResourceStatus = function()
  --   local key = pop_key()
  --   execute_tok'.Instances'
  --   local instances = pop_dict()
  --   local instance = instances[key]
  --   if instance then
  --     push(instance)
  --     return
  --   end
  --   push(key)
  --   ps_error'undefinedresource'
  -- end,
  -- ResourceForAll = function()
  --   local key = pop_key()
  --   execute_tok'.Instances'
  --   local instances = pop_dict()
  --   local instance = instances[key]
  --   if instance then
  --     push(instance)
  --     return
  --   end
  --   push(key)
  --   ps_error'undefinedresource'
  -- end,
}}

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
local any_object_or_end = any_object * l.Cp() + whitespace^-1 * -1 * l.Cc(nil) + l.Cp() * l.Cc(false)
function execute_string(str, context)
  local pos = 1
  while true do
    local tok
    tok, pos = any_object_or_end:match(str, pos)
    if pos then
      local success, err = pcall(execute_tok, tok, true)
      if not success then
        if context and type(err) == 'table' and err.pserror and not err.context then
          err.tok = tok
          err.context = context
        end
        error(err)
      end
    elseif pos == false then
      ps_error'syntaxerror'
    else
      break
    end
  end
end

--[[
  local readstate = status.readstate or status
  local context = string.format('%s:%i', readstate.filename, readstate.linenumber)
  local tokens = token.scan_argument(true)
  local n = node.new('whatsit', late_lua_sub)
  setwhatsitfield(n, 'data', function()
    assert(not ps_tokens)
    ps_tokens = tokens
    ps_context = context
  end)
  local nn = node.new('glyph')
  nn.subtype = 256
  nn.font, nn.char = fid, 0x1F3A8
  n.next = nn
  if tex.nest.ptr == 0 then
    -- Main vertical list. Here we might appear before the page starts properly
    -- and should not freeze page specifications. Since we don't have any outer dimensions,
    -- we can ensure this by sneaking our node into the current page list whithout going though
    -- build_page.
    tex.triggerbuildpage() -- First ensure that everything else is contributed properly.
    tex.lists.page_head = node.insert_after(tex.lists.page_head, nil, n)
  else
    node.write(n)
  end
end
]]
local func = luatexbase.new_luafunction'luaPSTheader'
token.set_lua('luaPSTheader', func, 'protected')
lua.get_functions_table()[func] = function()
  local is_inline = token.scan_keyword'inline'
  local readstate = status.readstate or status
  local context = is_inline and string.format('%s:%i', readstate.filename, readstate.linenumber)
  local data = token.scan_argument(true)
  local n = node.new('whatsit', late_lua_sub)
  setwhatsitfield(n, 'data', function()
    if not is_inline then
      context = data
      local f = io.open(kpse.find_file(data, 'PostScript header'), 'r')
      data = f:read'a'
      f:close()
    end
    local stack_depth = #operand_stack

    local x, y = pdf.getpos()
    local height = #operand_stack
    operand_stack[height + 1], operand_stack[height + 2] = x/65781.76, y/65781.76
    systemdict.value.moveto()

    local saved_pdfprint = pdfprint
    pdfprint = function(s) return pdf.print('direct', s .. '\n') end
    execute_string(data, context)
    flush_delayed()
    pdfprint = saved_pdfprint
    if #operand_stack ~= stack_depth then
      error'Unexpected values on operand stack'
    end
  end)
  node.write(n)
end

--[[
local func = luatexbase.new_luafunction'showPS'
token.set_lua('showPS', func, 'protected')
lua.get_functions_table()[func] = function()
  local command = token.scan_argument(true)
  -- This will break if any graphics commands are used.
  local tokens = parse_ps(command)
  execute_ps(tokens)
  systemdict.value.stack()
end
]]

local ps_tokens, ps_direct, ps_context, ps_pos_x, ps_pos_y
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
          assert(not ps_pos_x)
          ps_pos_x, ps_pos_y = pdf.getpos()
          n.xoffset = -ps_pos_x
          n.yoffset = -ps_pos_y
          n = node.hpack(n)
          vf.node(node.direct.todirect(n))
          node.free(n)
        end}
      }
    },
    [1] = {
      commands = {
        {'lua', function()
          local tokens, direct = assert(ps_tokens), ps_direct
          ps_tokens = nil
          local x, y = pdf.getpos()
          local TeXDict = userdict.value.TeXDict.value
          local saved_ocount = TeXDict.ocount
          local height = #operand_stack
          TeXDict.ocount = height
          operand_stack[height + 1], operand_stack[height + 2] = ps_pos_x/65781.76, ps_pos_y/65781.76
          ps_pos_x, ps_pos_y = nil
          local graphics_height
          if direct then
            systemdict.value.moveto()
          else
            graphics_height = #graphics_stack
            systemdict.value.gsave()
            systemdict.value.translate()
          end
          local success, err = pcall(execute_string, tokens, ps_context)
          if not success then
            if type(err) == 'table' and err.pserror then
              tex.error(string.format('luapstricks: %q error occured while executing PS code from %q', err.pserror, err.context), {
                string.format('The error occured while executing the PS command %q.\n%s', err.tok, err.trace)
              })
            else
              error(err, 0)
            end
          end
          flush_delayed()
          if not direct then
            systemdict.value.grestore()
            if graphics_height ~= #graphics_stack then
              if graphics_height < #graphics_stack then
                texio.write_nl"luapstricks: PS block contains unbalanced gsave. grestore will be executed to compensate."
                repeat
                  systemdict.value.grestore()
                until graphics_height == #graphics_stack
              else
                texio.write_nl"luapstricks: PS block contains unbalanced grestore."
              end
            end
            height = TeXDict.ocount or height
            local new_height = #operand_stack
            assert(new_height >= height)
            for k = height + 1, new_height do
              operand_stack[k] = nil
            end
            TeXDict.ocount = saved_ocount
          end
        end}
      }
    },
  },
}

local modes = tex.getmodevalues()
local func = luatexbase.new_luafunction'luaPST'
token.set_lua('luaPST', func, 'protected')
lua.get_functions_table()[func] = function()
  local readstate = status.readstate or status
  local context = string.format('%s:%i', readstate.filename, readstate.linenumber)
  local direct = token.scan_keyword'direct'
  local tokens = token.scan_argument(true)
  local n = node.new('whatsit', late_lua_sub)
  setwhatsitfield(n, 'data', function(n)
    assert(not ps_tokens)
    ps_tokens = tokens
    ps_direct = direct
    ps_context = context

    local nn = node.new('glyph')
    nn.subtype = 256
    nn.font, nn.char = fid, 0x1F3A8
    local list = node.new('hlist')
    list.head = nn
    list.direction = 0
    node.insert_after(n, n, list)
  end)
  node.write(n)
end

do
  func = luatexbase.new_luafunction'luaPSTcolor'
  token.set_lua('luaPSTcolor', func)
  local ps_rgb = 'rgb ' * l.C(l.P(1)^0) * l.Cc' setrgbcolor' * l.Cc'rgb '
  local ps_cmyk = 'cmyk ' * l.C(l.P(1)^0) * l.Cc' setcmykcolor' * l.Cc'cmyk '
  local ps_gray = 'gray ' * l.C(l.P(1)^0) * l.Cc' setgray' * l.Cc'gray '
  local ps_hsb = 'hsb ' * l.C(l.P(1)^0) * l.Cc' sethsbcolor' * l.Cc'hsb '
  local pscolor = ps_rgb + ps_gray + ps_gray + ps_hsb
  local pdf_rgb = l.Cmt(l.C(number * whitespace * number * whitespace * number / 0) * whitespace * 'rg'
                * whitespace * l.C(number * whitespace * number * whitespace * number / 0) * whitespace * 'RG' * -1, function(s, p, a, b)
                  if a == b then
                    return true, a, ' setrgbcolor', 'rgb '
                  else
                    return false
                  end
                end)
  local pdf_cmyk = l.Cmt(l.C(number * whitespace * number * whitespace * number * whitespace * number / 0) * whitespace * 'k'
                 * whitespace * l.C(number * whitespace * number * whitespace * number * whitespace * number / 0) * whitespace * 'K' * -1, function(s, p, a, b)
                  if a == b then
                    return true, a, ' setcmykcolor', 'cmyk '
                  else
                    return false
                  end
                end)
  local pdf_gray = l.Cmt(l.C(number / 0) * whitespace * 'g'
                 * whitespace * l.C(number / 0) * whitespace * 'G' * -1, function(s, p, a, b)
                  if a == b then
                    return true, a, ' setgray', 'gray '
                  else
                    return false
                  end
                end)
  local pdf_other = l.Cs(l.Cc'(' * l.P(1)^0 * l.Cc')') * l.C' setpdfcolor' * l.C'gray '
  local pdfcolor = pdf_rgb + pdf_cmyk + pdf_gray + pdf_other
  local anycolor = pscolor + pdfcolor
  lua.get_functions_table()[func] = function()
    local dvips_format = token.scan_keyword'dvips'
    local result, suffix, prefix = anycolor:match(token.scan_argument())
    tex.sprint(-2, dvips_format and prefix .. result or result .. suffix)
  end
end

func = luatexbase.new_luafunction'luaPSTbox'
token.set_lua('luaPSTbox', func)
lua.get_functions_table()[func] = function()
  local box = register_texbox(token.scan_list())
  tex.sprint(-2, tostring(box))
end
