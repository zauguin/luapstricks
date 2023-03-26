---- luapstricks-plugin-pstmarble.lua
-- Copyright 2021--2023 Marcel Krüger <tex@2krueger.de>
-- Converted from PostScript in pst-marble.pro version 1.6
-- pst-marble.pro: Copyright 2018--2019 Aubrey Jaffer
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
-- This work consists of the files luapstricks.lua and luapstricks-plugin-pstmarble.lua

local loader, version, plugininterface = ...
assert(loader == 'luapstricks' and version == 0)

local push = plugininterface.push
local pop = plugininterface.pop
local pop_array = plugininterface.pop_array
local pop_num = plugininterface.pop_num
local pop_proc = plugininterface.pop_proc
local exec = plugininterface.exec

local abs = math.abs
local exp = math.exp
local cos = math.cos
local sin = math.sin
local rad = math.rad
local max = math.max
local deg = math.deg
local atan = math.atan

local function spread(px, py, cx, cy, rad2)
  local pc2 = (px - cx)^2 + (py - cy)^2 -- (p-c)^2
  local a = (rad2 / pc2 + 1)^0.5
  return (px - cx) * a + cx, (py - cy) * a + cy
end

local e_inv = math.exp(-1)
local function rake_deformation(px, py, dx, dy, rs, V, tU, Linv)
  local a = 0
  for i = 1, #rs do
    local r = rs[i]
    local bx, by = dy * r, -dx * r
    a = a + exp(-abs((px - bx) * dy - (py - by) * dx) * Linv) * tU
  end
  return px + dx * a, py + dy * a
end

local function stir_deformation(px, py, cx, cy, rs, th, Linv, oversample)
  local dx, dy = (px - cx), (py - cy)
  local dist = (dx^2 + dy^2)^.5
  if dist <= 1e-6 then return px, py end

  local a = 0
  for i = 1, #rs do
    local r = rs[i]
    local positive = r > 0
    if not positive then r = -r end
    local delta = exp(-abs(dist - r) * (Linv / r)) * th
    if positive then
      a = a - delta
    else
      a = a + delta
    end
  end
  if oversample > 0 then
    a = -a
  end
  a = rad(a)
  local cos_a, sin_a = cos(a), sin(a)
  return cos_a * dx - sin_a * dy + cx, sin_a * dx + cos_a * dy + cy
end

local function symmod(x, m)
  local x = x % m
  if 2 * x >= m then
    x = x - m
  end
  return x
end

-- Common code to compute inverse of non-linear deformation
local function g1(mdls, a, mf, af, major, pw, freq)
  local tmp = mdls / 2
  if a < 0 then tmp = -tmp end

  local tmp2
  if mf > 0 then
    tmp2 = 1 - max(1 - abs(af / 180), 0)^pw
  else
    tmp2 = abs(af / 180)^pw
  end
  local g0 = tmp * tmp2
--[[
    %% one iteration of Newton-Raphson
    %% g_1=g_0-(g_0-a+(m/2)*sin(g_0*f))/(1+pi*m*f/360*cos(g_0*f))
]]
  local gf = rad(g0 * freq)
  return g0 - (g0 - a + major * sin(gf)) / (1 + mf/2 * cos(gf))
end

local function jiggle_deformation(px, py, dx, dy, freq, ofst, trv, major, minor, mf, mdls, pw)
  local a = symmod(py * dx + px * dy + ofst, mdls)
  local af = a * freq
  local x, y
  if mf ~= 0 then
    --[[
      % find the minor axis displacement from the major axis value
      % where it was moved from.
    ]]
    local g = g1(mdls, a, mf, af, major, pw, freq)
    x, y = g - a, cos(rad(g * freq)) * minor
  else
    local ang = rad(af)
    x, y = sin(ang), -cos(ang)
    -- x, y = x * major, y * minor
  end
  return trv[1] * x + trv[3] * y + px, trv[2] * x + trv[4] * y + py
end

local function wriggle_deformation(px, py, cx, cy, freq, major, minor, mf, mdls, pw)
  local rd = ((px - cx)^2 + (py - cy)^2)^.5
  if rd <= 1e-6 then return px, py end

  local a = symmod(rd, mdls)
  local af = a * freq

  -- x, y are radial and angular displacements from cx,cy
  -- The naming is used to demonstrate the similarity with jiggle.
  local x, y
  if mf ~= 0 then
    local g = g1(mdls, a, mf, af, major, pw, freq)
    x, y = g - a, cos(rad(g * freq)) * minor
  else
    local ang = rad(af)
    x, y = sin(ang) * major, -cos(ang) * minor
  end
  rd = rd + x
  local ang = rad(y) + atan(px - cx, py - cy)

  return sin(ang) * rd + cx, cos(ang) * rd + cy
end

local function stylus_deformation(px, py, bx, by, ex, ey, L, tU, steps, nx, ny, step_x, step_y)
  for _ = 1, steps do
    local dxB, dyB = bx - px, by - py
    local dxE, dyE = ex - px, ey - py
    local r = (dxB^2 + dyB^2)^.5
    local denr = r / L
    if 0 < denr and denr < 6 then
      local s = (dxE^2 + dyE^2)^.5
      local txB = dxB * nx + dyB * ny
      local txE = dxE * nx + dyE * ny
      local ty = dxB * ny - dyB * nx
      denr = 2*L*r * exp(denr)
      local dens = 2*L*s * exp(s / L)
      local ty2 = ty^2
      local inx = (L*r - ty2) * tU / denr
                + (L*s - ty2) * tU / dens
      local iny = txB * ty    * tU / denr
                + txE * ty    * tU / dens
      px = px + inx * nx + iny * ny
      py = py + inx * ny - iny * nx
    end
    bx, by = ex, ey
    ex, ey = ex + step_x, ey + step_y
  end
  return px, py
end

-- An irrotational vortex.  circ is circulation; t is time in seconds
local m4o3 = -4/3
local function vortex_deformation(px, py, cx, cy, circ, t, nuterm)
  local pc2 = (px - cx)^2 + (py - cy)^2
  if pc2 < 1e-6 then return px, py end
  local a = rad((nuterm + (pc2 * t)^.75)^m4o3 * circ)
  px, py = px - cx, py - cy
  local cos_a, sin_a = cos(a), sin(a)
  return cos_a * px - sin_a * py + cx, sin_a * px + cos_a * py + cy
end

-- We don't actually gain much from moving this one to Lua, but it's more consistent
local function offset_deformation(px, py, dx, dy)
  return px + dx, py + dy
end

local function do_turn(px, py, cx, cy, trv)
  px, py = px - cx, py - cy
  return trv[1] * px + trv[3] * py + trv[5], trv[2] * px + trv[4] * py + trv[6]
end

local function ct_handler(handler)
  return function(px, py, args, count)
    push(px)
    push(py)

    for j = 1, count do
      push(args[j])
    end
    exec(handler)
    py = pop_num()
    px = pop_num()
    return px, py
  end
end

local function ct_dispatch_fallback(fallback)
  local handler = ct_handler(fallback)
  return function(ct, px, py, args, count)
    push{kind = 'name', value = 'ct'}
    push{kind = 'name', value = ct}
    exec'def'

    return handler(px, py, args, count)
  end
end

local ct_handlers = {
  offset = function(px, py, args, count)
    assert(count == 2)
    return offset_deformation(px, py, args[1], args[2])
  end,
  -- offset = ct_handler'offset-deformation',
  turn = function(px, py, args, count)
    assert(count == 3)
    return do_turn(px, py, args[1], args[2], args[3].value)
  end,
  -- turn = ct_handler'do-turn',
  jiggle = function(px, py, args, count)
    assert(count == 10)
    return jiggle_deformation(px, py, args[1], args[2], args[3], args[4], args[5].value, args[6], args[7], args[8], args[9], args[10])
  end,
  -- jiggle = ct_handler'jiggle-deformation',
  rake = function(px, py, args, count)
    assert(count == 6)
    return rake_deformation(px, py, args[1], args[2], args[3].value, args[4], args[5], args[6])
  end,
  -- rake = ct_handler'rake-deformation',
  vortex = function(px, py, args, count)
    assert(count == 4)
    exec'nuterm'
    local nuterm = pop_num()
    return vortex_deformation(px, py, args[1], args[2], args[3], args[4], nuterm)
  end,
  -- vortex = ct_handler'vortex-deformation',
  stir = function(px, py, args, count)
    assert(count == 5)
    exec'oversample'
    local oversample = pop_num()
    return stir_deformation(px, py, args[1], args[2], args[3].value, args[4], args[5], oversample)
  end,
  -- stir = ct_handler'stir-deformation',
  wriggle = function(px, py, args, count)
    assert(count == 8)
    return wriggle_deformation(px, py, args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8])
  end,
  -- wriggle = ct_handler'wriggle-deformation',
  stylus = function(px, py, args, count)
    assert(count == 11)
    return stylus_deformation(px, py, args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11])
  end,
  -- stylus = ct_handler'stylus-deformation',
}
local function ct_dispatch(fallback)
  fallback = ct_dispatch_fallback(fallback)
  return function(ct, px, py, args, count)
    local handler = ct_handlers[ct]
    if handler then
      return handler(px, py, args, count)
    else
      return fallback(ct, px, py, args, count)
    end
  end
end

local function sharpen(x)
  x = x - 0.5
  if abs(x) >= 1e-8 then
    x = x / abs(x)^.66 * .63
  end
  return x + 0.5
end

local newtable = lua.newtable
local function Vmap2(v1, v2, func)
  local result = newtable(#v2, 0)
  for i=1, #v1 do
    result[i] = func(v1[i], v2[i])
  end
  return result
end

local function paper_shading(rgb, pwr, paper)
  return Vmap2(rgb, paper, function(c1, c2)
    if c2 >= c1 then
      local a = 1 - c1/c2
      if a >= 1e-30 then
        a = a^pwr
      end
      return (1 - a) * c2
    else
      local a = (1 - c1) / (1 - c2)
      if a >= 1e-30 then 
        a = a^(2-pwr)
      end
      return 1 - a * (1 - c2)
    end
  end)
end

local function actions2rgb(fallback)
  return function(px, py, actions, acnt, paper)
    local dispatch = ct_dispatch(fallback)
    local cdx = acnt
    for cdx = acnt, 1, -1 do
      local action = actions[cdx]
      local kind = action.kind
      if kind == 'executable' then
        action = action.value
        kind = action.kind
      end
      assert(kind == 'array')
      action = action.value
      local count = #action
      local ct = action[count].value
      if ct == 'drop' then
        assert(count == 8)
        local cx, cy = action[1], action[2]
        local rad2 = action[3]
        local bgc, rgb = action[4].value, action[5].value
        local sr2, gc = action[6], action[7]

        local a2 = (px - cx)^2 + (py - cy)^2
        local disc = a2 < 1e-10 and 0 or 1 - rad2 / a2
        if disc <= 0 then
          if gc ~= 0 then
            rgb = paper_shading(rgb, exp(a2 * sr2) * gc, paper)
          end
          if disc > -0.001 then
            local a = sharpen((-disc)^.5)
            rgb = Vmap2(rgb, bgc, function(v1, v2) return v1 * a + v2 * (1-a) end)
          end
          return rgb
        else
          local a = disc^.5
          px, py = (px - cx) * a + cx, (py - cy) * a + cy
        end
      else
        px, py = dispatch(ct, px, py, action, count - 1)
      end
    end
  end
end

return {
  spread = function()
    local rad2 = pop_num() -- rad^2
    local cy = pop_num()
    local cx = pop_num()
    local py = pop_num()
    local px = pop_num()
    px, py = spread(px, py, cx, cy, rad2)
    push(px)
    push(py)
  end,
  ['.actions2rgb'] = function() -- px py actions acnt paper fallback .composite-map exec
    local _, fallback = pop_proc()
    local actions2rgb = actions2rgb(fallback)
    push(function()
      local paper = pop_array().value
      local acnt = pop_num()
      local actions = pop_array().value

      local py = pop_num()
      local px = pop_num()

      local rgb = actions2rgb(px, py, actions, acnt, paper)
      if rgb then
        push{kind = 'array', value = rgb}
        push(true)
      else
        push(false)
      end
    end)
  end,
  ['.paper-shading'] = function() -- rgb pwr paper
    local paper = pop_array().value
    local pwr = pop_num()
    local rgb = pop_array().value
    rgb = paper_shading(rgb, pwr, paper)
    push{kind = 'array', value = rgb}
  end,
  ['.composite-map'] = function() -- acnt idx actions fallback .composite-map exec
    local _, fallback = pop_proc()
    local dispatch = ct_dispatch(fallback)
    push(function()
      local actions = pop_array().value
      local idx = pop_num()
      local acnt = pop_num()

      local py = pop_num()
      local px = pop_num()

      for i = idx + 1, acnt - 1 do
        local action = actions[i+1]
        local kind = action.kind
        if kind == 'executable' then
          action = action.value
          kind = action.kind
        end
        assert(kind == 'array')
        action = action.value
        local count = #action
        local ct = action[count].value
        if ct == 'drop' then
          assert(count == 8)
          px, py = spread(px, py, action[1], action[2], action[3])
        else
          px, py = dispatch(ct, px, py, action, count - 1)
        end
      end
      push(px)
      push(py)
    end)
  end,
  ['stir-deformation'] = function()
    local Linv = pop_num()
    local th = pop_num()
    local rs = pop_array().value
    local cy = pop_num()
    local cx = pop_num()
    local py = pop_num()
    local px = pop_num()

    exec'oversample'
    local oversample = pop_num()
    px, py = stir_deformation(px, py, cx, cy, rs, th, Linv, oversample)
    push(px)
    push(py)
  end,
  ['rake-deformation'] = function()
    local Linv = pop_num()
    local tU = pop_num()
    local V = pop_num()
    local rs = pop_array().value
    local dy = pop_num()
    local dx = pop_num()
    local py = pop_num()
    local px = pop_num()
    px, py = rake_deformation(px, py, dx, dy, rs, V, tU, Linv)
    push(px)
    push(py)
  end,
  ['jiggle-deformation'] = function()
    local pw = pop_num()
    local mdls = pop_num()
    local mf = pop_num()
    local minor = pop_num()
    local major = pop_num()
    local trv = pop_array().value
    local ofst = pop_num()
    local freq = pop_num()
    local dy = pop_num()
    local dx = pop_num()
    local py = pop_num()
    local px = pop_num()
    px, py = jiggle_deformation(px, py, dx, dy, freq, ofst, trv, major, minor, mf, mdls, pw)
    push(px)
    push(py)
  end,
  ['wriggle-deformation'] = function()
    local pw = pop_num()
    local mdls = pop_num()
    local mf = pop_num()
    local minor = pop_num()
    local major = pop_num()
    local freq = pop_num()
    local cy = pop_num()
    local cx = pop_num()
    local py = pop_num()
    local px = pop_num()
    px, py = wriggle_deformation(px, py, cx, cy, freq, major, minor, mf, mdls, pw)
    push(px)
    push(py)
  end,
  ['stylus-deformation'] = function()
    local step_y = pop_num()
    local step_x = pop_num()
    local ny = pop_num()
    local nx = pop_num()
    local steps = pop_num()
    local tU = pop_num()
    local L = pop_num()
    local ey = pop_num()
    local ex = pop_num()
    local by = pop_num()
    local bx = pop_num()
    local py = pop_num()
    local px = pop_num()
    px, py = stylus_deformation(px, py, bx, by, ex, ey, L, tU, steps, nx, ny, step_x, step_y)
    push(px)
    push(py)
  end,
  ['.vortex-deformation'] = function()
    local nuterm = pop_num()
    local t = pop_num()
    local circ = pop_num()
    local cy = pop_num()
    local cx = pop_num()
    local py = pop_num()
    local px = pop_num()

    px, py = vortex_deformation(px, py, cx, cy, circ, t, nuterm)
    push(px)
    push(py)
  end,
  ['offset-deformation'] = function()
    local dy = pop_num()
    local dx = pop_num()
    local py = pop_num()
    local px = pop_num()

    px, py = offset_deformation(px, py, dx, dy)
    push(px)
    push(py)
  end,
  ['do-turn'] = function()
    local trv = pop_array().value
    local cy = pop_num()
    local cx = pop_num()
    local py = pop_num()
    local px = pop_num()

    px, py = do_turn(px, py, cx, cy, trv)
    push(px)
    push(py)
  end,
  ['.Minsky-circle'] = function()
    local eps = pop_num()
    local y = pop_num()
    local x = pop_num()
    x = x - eps * y
    y = y + eps * x
    push(x)
    push(y)
  end,
}, 0
