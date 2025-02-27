if _NAME then
    return client.error_log '[re] segmentation fault'
end

local initializeComponent, script_name, targetSize = ...

local base64Lib
do
    local M = {}

    local shl, shr, band = bit.lshift, bit.rshift, bit.band
    local char, byte, gsub, sub, format, concat, tostring, error, pairs = string.char, string.byte, string.gsub,
        string.sub, string.format, table.concat, tostring, error, pairs

    local extract = function(v, from, width)
        return band(shr(v, from), shl(1, width) - 1)
    end

    local function makeencoder(alphabet)
        local encoder, decoder = {}, {}
        for i = 1, 65 do
            local chr = byte(sub(alphabet, i, i)) or 32 -- or " "
            if decoder[chr] ~= nil then
                error("invalid alphabet: duplicate character " .. tostring(chr), 3)
            end
            encoder[i - 1] = chr
            decoder[chr] = i - 1
        end
        return encoder, decoder
    end

    local encoders, decoders = {}, {}

    encoders["base64"], decoders["base64"] = makeencoder(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=")
    encoders["base64url"], decoders["base64url"] = makeencoder(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")

    local alphabet_mt = {
        __index = function(tbl, key)
            if type(key) == "string" and key:len() == 64 or key:len() == 65 then
                encoders[key], decoders[key] = makeencoder(key)
                return tbl[key]
            end
        end
    }

    setmetatable(encoders, alphabet_mt)
    setmetatable(decoders, alphabet_mt)

    function M.encode(str, encoder)
        encoder = encoders[encoder or "base64"] or error("invalid alphabet specified", 2)

        str = tostring(str)

        local t, k, n = {}, 1, #str
        local lastn = n % 3
        local cache = {}

        for i = 1, n - lastn, 3 do
            local a, b, c = byte(str, i, i + 2)
            local v = a * 0x10000 + b * 0x100 + c
            local s = cache[v]

            if not s then
                s = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)],
                    encoder[extract(v, 0, 6)])
                cache[v] = s
            end

            t[k] = s
            k = k + 1
        end

        if lastn == 2 then
            local a, b = byte(str, n - 1, n)
            local v = a * 0x10000 + b * 0x100
            t[k] = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[extract(v, 6, 6)], encoder[64])
        elseif lastn == 1 then
            local v = byte(str, n) * 0x10000
            t[k] = char(encoder[extract(v, 18, 6)], encoder[extract(v, 12, 6)], encoder[64], encoder[64])
        end

        return concat(t)
    end

    function M.decode(b64, decoder)
        decoder = decoders[decoder or "base64"] or error("invalid alphabet specified", 2)

        local pattern = "[^%w%+%/%=]"
        if decoder then
            local s62, s63
            for charcode, b64code in pairs(decoder) do
                if b64code == 62 then
                    s62 = charcode
                elseif b64code == 63 then
                    s63 = charcode
                end
            end
            pattern = format("[^%%w%%%s%%%s%%=]", char(s62), char(s63))
        end

        b64 = gsub(tostring(b64), pattern, '')

        local cache = {}
        local t, k = {}, 1
        local n = #b64
        local padding = sub(b64, -2) == "==" and 2 or sub(b64, -1) == "=" and 1 or 0

        for i = 1, padding > 0 and n - 4 or n, 4 do
            local a, b, c, d = byte(b64, i, i + 3)

            local v0 = a * 0x1000000 + b * 0x10000 + c * 0x100 + d
            local s = cache[v0]
            if not s then
                local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40 + decoder[d]
                s = char(extract(v, 16, 8), extract(v, 8, 8), extract(v, 0, 8))
                cache[v0] = s
            end

            t[k] = s
            k = k + 1
        end

        if padding == 1 then
            local a, b, c = byte(b64, n - 3, n - 1)
            local v = decoder[a] * 0x40000 + decoder[b] * 0x1000 + decoder[c] * 0x40
            t[k] = char(extract(v, 16, 8), extract(v, 8, 8))
        elseif padding == 2 then
            local a, b = byte(b64, n - 3, n - 2)
            local v = decoder[a] * 0x40000 + decoder[b] * 0x1000
            t[k] = char(extract(v, 16, 8))
        end
        return concat(t)
    end

    base64Lib = M
end

local base64
do
    local Cipher_code = '2d 49 4f 41 4a 65 69 30 6d 69 30 65 4e 2a 44 29 55 4e 40 23 6f 64 6d 51 4c 44 40 23 4a 4e 75 69'

    local b = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
    local function Xor(a, b)
        local c; local d = ""
        for e = 1, string.len(a), 1 do
            c = e; if string.len(b) < c then c = c % string.len(b) end; d = d ..
                string.char(bit.bxor(string.byte(string.sub(a, e, e)), string.byte(string.sub(b, c, c))))
        end; return d
    end

    base64 = {
        encode = function(a)
            return tostring(base64Lib.encode(Xor(a, Cipher_code)))
        end,

        decode = function(a)
            return tostring(Xor(base64Lib.decode(a), Cipher_code))
        end
    }
end

local bit = require(base64.decode 'UA1U')
local json
do
    json = { _version = "0.1.2" }

    local encode

    local escape_char_map = {
        ["\\"] = "\\",
        ["\""] = "\"",
        ["\b"] = "b",
        ["\f"] = "f",
        ["\n"] = "n",
        ["\r"] = "r",
        ["\t"] = "t",
    }

    local escape_char_map_inv = { ["/"] = "/" }
    for k, v in pairs(escape_char_map) do
        escape_char_map_inv[v] = k
    end


    local function escape_char(c)
        return "\\" .. (escape_char_map[c] or string.format("u%04x", c:byte()))
    end


    local function encode_nil(val)
        return "null"
    end


    local function encode_table(val, stack)
        local res = {}
        stack = stack or {}

        -- Circular reference?
        if stack[val] then error("circular reference") end

        stack[val] = true

        if rawget(val, 1) ~= nil or next(val) == nil then
            -- Treat as array -- check keys are valid and it is not sparse
            local n = 0
            for k in pairs(val) do
                if type(k) ~= "number" then
                    error("invalid table: mixed or invalid key types")
                end
                n = n + 1
            end
            if n ~= #val then
                error("invalid table: sparse array")
            end
            -- Encode
            for i, v in ipairs(val) do
                table.insert(res, encode(v, stack))
            end
            stack[val] = nil
            return "[" .. table.concat(res, ",") .. "]"
        else
            -- Treat as an object
            for k, v in pairs(val) do
                if type(k) ~= "string" then
                    error("invalid table: mixed or invalid key types")
                end
                table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
            end
            stack[val] = nil
            return "{" .. table.concat(res, ",") .. "}"
        end
    end


    local function encode_string(val)
        return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
    end


    local function encode_number(val)
        -- Check for NaN, -inf and inf
        if val ~= val or val <= -math.huge or val >= math.huge then
            error("unexpected number value '" .. tostring(val) .. "'")
        end
        return string.format("%.14g", val)
    end


    local type_func_map = {
        ["nil"] = encode_nil,
        ["table"] = encode_table,
        ["string"] = encode_string,
        ["number"] = encode_number,
        ["boolean"] = tostring,
    }


    encode = function(val, stack)
        local t = type(val)
        local f = type_func_map[t]
        if f then
            return f(val, stack)
        end
        error("unexpected type '" .. t .. "'")
    end


    function json.encode(val)
        return (encode(val))
    end

    -------------------------------------------------------------------------------
    -- Decode
    -------------------------------------------------------------------------------

    local parse

    local function create_set(...)
        local res = {}
        for i = 1, select("#", ...) do
            res[select(i, ...)] = true
        end
        return res
    end

    local space_chars  = create_set(" ", "\t", "\r", "\n")
    local delim_chars  = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
    local escape_chars = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
    local literals     = create_set("true", "false", "null")

    local literal_map  = {
        ["true"] = true,
        ["false"] = false,
        ["null"] = nil,
    }


    local function next_char(str, idx, set, negate)
        for i = idx, #str do
            if set[str:sub(i, i)] ~= negate then
                return i
            end
        end
        return #str + 1
    end


    local function decode_error(str, idx, msg)
        local line_count = 1
        local col_count = 1
        for i = 1, idx - 1 do
            col_count = col_count + 1
            if str:sub(i, i) == "\n" then
                line_count = line_count + 1
                col_count = 1
            end
        end
        error(string.format("%s at line %d col %d", msg, line_count, col_count))
    end


    local function codepoint_to_utf8(n)
        -- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
        local f = math.floor
        if n <= 0x7f then
            return string.char(n)
        elseif n <= 0x7ff then
            return string.char(f(n / 64) + 192, n % 64 + 128)
        elseif n <= 0xffff then
            return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
        elseif n <= 0x10ffff then
            return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
                f(n % 4096 / 64) + 128, n % 64 + 128)
        end
        error(string.format("invalid unicode codepoint '%x'", n))
    end


    local function parse_unicode_escape(s)
        local n1 = tonumber(s:sub(1, 4), 16)
        local n2 = tonumber(s:sub(7, 10), 16)
        -- Surrogate pair?
        if n2 then
            return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
        else
            return codepoint_to_utf8(n1)
        end
    end


    local function parse_string(str, i)
        local res = ""
        local j = i + 1
        local k = j

        while j <= #str do
            local x = str:byte(j)

            if x < 32 then
                decode_error(str, j, "control character in string")
            elseif x == 92 then -- `\`: Escape
                res = res .. str:sub(k, j - 1)
                j = j + 1
                local c = str:sub(j, j)
                if c == "u" then
                    local hex = str:match("^[dD][89aAbB]%x%x\\u%x%x%x%x", j + 1)
                        or str:match("^%x%x%x%x", j + 1)
                        or decode_error(str, j - 1, "invalid unicode escape in string")
                    res = res .. parse_unicode_escape(hex)
                    j = j + #hex
                else
                    if not escape_chars[c] then
                        decode_error(str, j - 1, "invalid escape char '" .. c .. "' in string")
                    end
                    res = res .. escape_char_map_inv[c]
                end
                k = j + 1
            elseif x == 34 then -- `"`: End of string
                res = res .. str:sub(k, j - 1)
                return res, j + 1
            end

            j = j + 1
        end

        decode_error(str, i, "expected closing quote for string")
    end


    local function parse_number(str, i)
        local x = next_char(str, i, delim_chars)
        local s = str:sub(i, x - 1)
        local n = tonumber(s)
        if not n then
            decode_error(str, i, "invalid number '" .. s .. "'")
        end
        return n, x
    end


    local function parse_literal(str, i)
        local x = next_char(str, i, delim_chars)
        local word = str:sub(i, x - 1)
        if not literals[word] then
            decode_error(str, i, "invalid literal '" .. word .. "'")
        end
        return literal_map[word], x
    end


    local function parse_array(str, i)
        local res = {}
        local n = 1
        i = i + 1
        while 1 do
            local x
            i = next_char(str, i, space_chars, true)
            -- Empty / end of array?
            if str:sub(i, i) == "]" then
                i = i + 1
                break
            end
            -- Read token
            x, i = parse(str, i)
            res[n] = x
            n = n + 1
            -- Next token
            i = next_char(str, i, space_chars, true)
            local chr = str:sub(i, i)
            i = i + 1
            if chr == "]" then break end
            if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
        end
        return res, i
    end


    local function parse_object(str, i)
        local res = {}
        i = i + 1
        while 1 do
            local key, val
            i = next_char(str, i, space_chars, true)
            -- Empty / end of object?
            if str:sub(i, i) == "}" then
                i = i + 1
                break
            end
            -- Read key
            if str:sub(i, i) ~= '"' then
                decode_error(str, i, "expected string for key")
            end
            key, i = parse(str, i)
            -- Read ':' delimiter
            i = next_char(str, i, space_chars, true)
            if str:sub(i, i) ~= ":" then
                decode_error(str, i, "expected ':' after key")
            end
            i = next_char(str, i + 1, space_chars, true)
            -- Read value
            val, i = parse(str, i)
            -- Set
            res[key] = val
            -- Next token
            i = next_char(str, i, space_chars, true)
            local chr = str:sub(i, i)
            i = i + 1
            if chr == "}" then break end
            if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
        end
        return res, i
    end


    local char_func_map = {
        ['"'] = parse_string,
        ["0"] = parse_number,
        ["1"] = parse_number,
        ["2"] = parse_number,
        ["3"] = parse_number,
        ["4"] = parse_number,
        ["5"] = parse_number,
        ["6"] = parse_number,
        ["7"] = parse_number,
        ["8"] = parse_number,
        ["9"] = parse_number,
        ["-"] = parse_number,
        ["t"] = parse_literal,
        ["f"] = parse_literal,
        ["n"] = parse_literal,
        ["["] = parse_array,
        ["{"] = parse_object,
    }


    parse = function(str, idx)
        local chr = str:sub(idx, idx)
        local f = char_func_map[chr]
        if f then
            return f(str, idx)
        end
        decode_error(str, idx, "unexpected character '" .. chr .. "'")
    end


    function json.parse(str)
        if type(str) ~= "string" then
            error("expected argument of type string, got " .. type(str))
        end
        local res, idx = parse(str, next_char(str, 1, space_chars, true))
        idx = next_char(str, idx, space_chars, true)
        if idx <= #str then
            decode_error(str, idx, "trailing garbage")
        end
        return res
    end

    local function kind_of(obj)
        if type(obj) ~= 'table' then return type(obj) end
        local i = 1
        for _ in pairs(obj) do
            if obj[i] ~= nil then i = i + 1 else return 'table' end
        end
        if i == 1 then return 'table' else return 'array' end
    end

    local function escape_str(s)
        local in_char  = { '\\', '"', '/', '\b', '\f', '\n', '\r', '\t' }
        local out_char = { '\\', '"', '/', 'b', 'f', 'n', 'r', 't' }
        for i, c in ipairs(in_char) do
            s = s:gsub(c, '\\' .. out_char[i])
        end
        return s
    end

    function json.stringify(obj, as_key)
        local s = {}              -- We'll build the string as an array of strings to be concatenated.
        local kind = kind_of(obj) -- This is 'array' if it's an array or type(obj) otherwise.
        if kind == 'array' then
            if as_key then error('Can\'t encode array as key.') end
            s[#s + 1] = '['
            for i, val in ipairs(obj) do
                if i > 1 then s[#s + 1] = ', ' end
                s[#s + 1] = json.stringify(val)
            end
            s[#s + 1] = ']'
        elseif kind == 'table' then
            if as_key then error('Can\'t encode table as key.') end
            s[#s + 1] = '{'
            for k, v in pairs(obj) do
                if #s > 1 then s[#s + 1] = ', ' end
                s[#s + 1] = json.stringify(k, true)
                s[#s + 1] = ':'
                s[#s + 1] = json.stringify(v)
            end
            s[#s + 1] = '}'
        elseif kind == 'string' then
            return '"' .. escape_str(obj) .. '"'
        elseif kind == 'number' then
            if as_key then return '"' .. tostring(obj) .. '"' end
            return tostring(obj)
        elseif kind == 'boolean' then
            return tostring(obj)
        elseif kind == 'nil' then
            return 'null'
        else
            error('Unjsonifiable type: ' .. kind .. '.')
        end
        return table.concat(s)
    end
end

local buildList = {
    [1] = 'stable',
    [2] = 'beta',
    [3] = 'debug'
}

local ffi = require(base64.decode 'VAJJ')
local http = require("gamesense/http")

local crypto
do
    local sha256 = {}

    local MOD = 2 ^ 32
    local MODM = MOD - 1

    local function memoize(f)
        local mt = {}
        local t = setmetatable({}, mt)
        function mt:__index(k)
            local v = f(k)
            t[k] = v
            return v
        end

        return t
    end

    local function make_bitop_uncached(t, m)
        local function bitop(a, b)
            local res, p = 0, 1
            while a ~= 0 and b ~= 0 do
                local am, bm = a % m, b % m
                res = res + t[am][bm] * p
                a = (a - am) / m
                b = (b - bm) / m
                p = p * m
            end
            res = res + (a + b) * p
            return res
        end
        return bitop
    end

    local function make_bitop(t)
        local op1 = make_bitop_uncached(t, 2 ^ 1)
        local op2 = memoize(function(a) return memoize(function(b) return op1(a, b) end) end)
        return make_bitop_uncached(op2, 2 ^ (t.n or 1))
    end

    local bxor1 = make_bitop({ [0] = { [0] = 0, [1] = 1 }, [1] = { [0] = 1, [1] = 0 }, n = 4 })

    local function bxor(a, b, c, ...)
        local z = nil
        if b then
            a = a % MOD
            b = b % MOD
            z = bxor1(a, b)
            if c then z = bxor(z, c, ...) end
            return z
        elseif a then
            return a % MOD
        else
            return 0
        end
    end

    local function band(a, b, c, ...)
        local z
        if b then
            a = a % MOD
            b = b % MOD
            z = ((a + b) - bxor1(a, b)) / 2
            if c then z = bit32_band(z, c, ...) end
            return z
        elseif a then
            return a % MOD
        else
            return MODM
        end
    end

    local function bnot(x) return (-1 - x) % MOD end

    local function rshift1(a, disp)
        if disp < 0 then return lshift(a, -disp) end
        return math.floor(a % 2 ^ 32 / 2 ^ disp)
    end

    local function rshift(x, disp)
        if disp > 31 or disp < -31 then return 0 end
        return rshift1(x % MOD, disp)
    end

    local function lshift(a, disp)
        if disp < 0 then return rshift(a, -disp) end
        return (a * 2 ^ disp) % 2 ^ 32
    end

    local function rrotate(x, disp)
        x = x % MOD
        disp = disp % 32
        local low = band(x, 2 ^ disp - 1)
        return rshift(x, disp) + lshift(low, 32 - disp)
    end

    local k = {
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
        0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
        0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
        0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
        0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
        0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
        0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
        0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    }

    local function str2hexa(s)
        return (string.gsub(s, ".", function(c) return string.format("%02x", string.byte(c)) end))
    end

    local function num2s(l, n)
        local s = ""
        for i = 1, n do
            local rem = l % 256
            s = string.char(rem) .. s
            l = (l - rem) / 256
        end
        return s
    end

    local function s232num(s, i)
        local n = 0
        for i = i, i + 3 do n = n * 256 + string.byte(s, i) end
        return n
    end

    local function preproc(msg, len)
        local extra = 64 - ((len + 9) % 64)
        len = num2s(8 * len, 8)
        msg = msg .. "\128" .. string.rep("\0", extra) .. len
        assert(#msg % 64 == 0)
        return msg
    end

    local function initH256(H)
        H[1] = 0x6a09e667
        H[2] = 0xbb67ae85
        H[3] = 0x3c6ef372
        H[4] = 0xa54ff53a
        H[5] = 0x510e527f
        H[6] = 0x9b05688c
        H[7] = 0x1f83d9ab
        H[8] = 0x5be0cd19
        return H
    end

    local function digestblock(msg, i, H)
        local w = {}
        for j = 1, 16 do w[j] = s232num(msg, i + (j - 1) * 4) end
        for j = 17, 64 do
            local v = w[j - 15]
            local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
            v = w[j - 2]
            w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
        end

        local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
        for i = 1, 64 do
            local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
            local maj = bxor(band(a, b), band(a, c), band(b, c))
            local t2 = s0 + maj
            local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
            local ch = bxor(band(e, f), band(bnot(e), g))
            local t1 = h + s1 + ch + k[i] + w[i]
            h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
        end

        H[1] = band(H[1] + a)
        H[2] = band(H[2] + b)
        H[3] = band(H[3] + c)
        H[4] = band(H[4] + d)
        H[5] = band(H[5] + e)
        H[6] = band(H[6] + f)
        H[7] = band(H[7] + g)
        H[8] = band(H[8] + h)
    end

    local function hex_to_binary(hex)
        return hex:gsub('..', function(hexval)
            return string.char(tonumber(hexval, 16))
        end)
    end

    local blocksize = 64 -- 512 bits

    local xor_with_0x5c = {
        [string.char(0)] = string.char(92),
        [string.char(1)] = string.char(93),
        [string.char(2)] = string.char(94),
        [string.char(3)] = string.char(95),
        [string.char(4)] = string.char(88),
        [string.char(5)] = string.char(89),
        [string.char(6)] = string.char(90),
        [string.char(7)] = string.char(91),
        [string.char(8)] = string.char(84),
        [string.char(9)] = string.char(85),
        [string.char(10)] = string.char(86),
        [string.char(11)] = string.char(87),
        [string.char(12)] = string.char(80),
        [string.char(13)] = string.char(81),
        [string.char(14)] = string.char(82),
        [string.char(15)] = string.char(83),
        [string.char(16)] = string.char(76),
        [string.char(17)] = string.char(77),
        [string.char(18)] = string.char(78),
        [string.char(19)] = string.char(79),
        [string.char(20)] = string.char(72),
        [string.char(21)] = string.char(73),
        [string.char(22)] = string.char(74),
        [string.char(23)] = string.char(75),
        [string.char(24)] = string.char(68),
        [string.char(25)] = string.char(69),
        [string.char(26)] = string.char(70),
        [string.char(27)] = string.char(71),
        [string.char(28)] = string.char(64),
        [string.char(29)] = string.char(65),
        [string.char(30)] = string.char(66),
        [string.char(31)] = string.char(67),
        [string.char(32)] = string.char(124),
        [string.char(33)] = string.char(125),
        [string.char(34)] = string.char(126),
        [string.char(35)] = string.char(127),
        [string.char(36)] = string.char(120),
        [string.char(37)] = string.char(121),
        [string.char(38)] = string.char(122),
        [string.char(39)] = string.char(123),
        [string.char(40)] = string.char(116),
        [string.char(41)] = string.char(117),
        [string.char(42)] = string.char(118),
        [string.char(43)] = string.char(119),
        [string.char(44)] = string.char(112),
        [string.char(45)] = string.char(113),
        [string.char(46)] = string.char(114),
        [string.char(47)] = string.char(115),
        [string.char(48)] = string.char(108),
        [string.char(49)] = string.char(109),
        [string.char(50)] = string.char(110),
        [string.char(51)] = string.char(111),
        [string.char(52)] = string.char(104),
        [string.char(53)] = string.char(105),
        [string.char(54)] = string.char(106),
        [string.char(55)] = string.char(107),
        [string.char(56)] = string.char(100),
        [string.char(57)] = string.char(101),
        [string.char(58)] = string.char(102),
        [string.char(59)] = string.char(103),
        [string.char(60)] = string.char(96),
        [string.char(61)] = string.char(97),
        [string.char(62)] = string.char(98),
        [string.char(63)] = string.char(99),
        [string.char(64)] = string.char(28),
        [string.char(65)] = string.char(29),
        [string.char(66)] = string.char(30),
        [string.char(67)] = string.char(31),
        [string.char(68)] = string.char(24),
        [string.char(69)] = string.char(25),
        [string.char(70)] = string.char(26),
        [string.char(71)] = string.char(27),
        [string.char(72)] = string.char(20),
        [string.char(73)] = string.char(21),
        [string.char(74)] = string.char(22),
        [string.char(75)] = string.char(23),
        [string.char(76)] = string.char(16),
        [string.char(77)] = string.char(17),
        [string.char(78)] = string.char(18),
        [string.char(79)] = string.char(19),
        [string.char(80)] = string.char(12),
        [string.char(81)] = string.char(13),
        [string.char(82)] = string.char(14),
        [string.char(83)] = string.char(15),
        [string.char(84)] = string.char(8),
        [string.char(85)] = string.char(9),
        [string.char(86)] = string.char(10),
        [string.char(87)] = string.char(11),
        [string.char(88)] = string.char(4),
        [string.char(89)] = string.char(5),
        [string.char(90)] = string.char(6),
        [string.char(91)] = string.char(7),
        [string.char(92)] = string.char(0),
        [string.char(93)] = string.char(1),
        [string.char(94)] = string.char(2),
        [string.char(95)] = string.char(3),
        [string.char(96)] = string.char(60),
        [string.char(97)] = string.char(61),
        [string.char(98)] = string.char(62),
        [string.char(99)] = string.char(63),
        [string.char(100)] = string.char(56),
        [string.char(101)] = string.char(57),
        [string.char(102)] = string.char(58),
        [string.char(103)] = string.char(59),
        [string.char(104)] = string.char(52),
        [string.char(105)] = string.char(53),
        [string.char(106)] = string.char(54),
        [string.char(107)] = string.char(55),
        [string.char(108)] = string.char(48),
        [string.char(109)] = string.char(49),
        [string.char(110)] = string.char(50),
        [string.char(111)] = string.char(51),
        [string.char(112)] = string.char(44),
        [string.char(113)] = string.char(45),
        [string.char(114)] = string.char(46),
        [string.char(115)] = string.char(47),
        [string.char(116)] = string.char(40),
        [string.char(117)] = string.char(41),
        [string.char(118)] = string.char(42),
        [string.char(119)] = string.char(43),
        [string.char(120)] = string.char(36),
        [string.char(121)] = string.char(37),
        [string.char(122)] = string.char(38),
        [string.char(123)] = string.char(39),
        [string.char(124)] = string.char(32),
        [string.char(125)] = string.char(33),
        [string.char(126)] = string.char(34),
        [string.char(127)] = string.char(35),
        [string.char(128)] = string.char(220),
        [string.char(129)] = string.char(221),
        [string.char(130)] = string.char(222),
        [string.char(131)] = string.char(223),
        [string.char(132)] = string.char(216),
        [string.char(133)] = string.char(217),
        [string.char(134)] = string.char(218),
        [string.char(135)] = string.char(219),
        [string.char(136)] = string.char(212),
        [string.char(137)] = string.char(213),
        [string.char(138)] = string.char(214),
        [string.char(139)] = string.char(215),
        [string.char(140)] = string.char(208),
        [string.char(141)] = string.char(209),
        [string.char(142)] = string.char(210),
        [string.char(143)] = string.char(211),
        [string.char(144)] = string.char(204),
        [string.char(145)] = string.char(205),
        [string.char(146)] = string.char(206),
        [string.char(147)] = string.char(207),
        [string.char(148)] = string.char(200),
        [string.char(149)] = string.char(201),
        [string.char(150)] = string.char(202),
        [string.char(151)] = string.char(203),
        [string.char(152)] = string.char(196),
        [string.char(153)] = string.char(197),
        [string.char(154)] = string.char(198),
        [string.char(155)] = string.char(199),
        [string.char(156)] = string.char(192),
        [string.char(157)] = string.char(193),
        [string.char(158)] = string.char(194),
        [string.char(159)] = string.char(195),
        [string.char(160)] = string.char(252),
        [string.char(161)] = string.char(253),
        [string.char(162)] = string.char(254),
        [string.char(163)] = string.char(255),
        [string.char(164)] = string.char(248),
        [string.char(165)] = string.char(249),
        [string.char(166)] = string.char(250),
        [string.char(167)] = string.char(251),
        [string.char(168)] = string.char(244),
        [string.char(169)] = string.char(245),
        [string.char(170)] = string.char(246),
        [string.char(171)] = string.char(247),
        [string.char(172)] = string.char(240),
        [string.char(173)] = string.char(241),
        [string.char(174)] = string.char(242),
        [string.char(175)] = string.char(243),
        [string.char(176)] = string.char(236),
        [string.char(177)] = string.char(237),
        [string.char(178)] = string.char(238),
        [string.char(179)] = string.char(239),
        [string.char(180)] = string.char(232),
        [string.char(181)] = string.char(233),
        [string.char(182)] = string.char(234),
        [string.char(183)] = string.char(235),
        [string.char(184)] = string.char(228),
        [string.char(185)] = string.char(229),
        [string.char(186)] = string.char(230),
        [string.char(187)] = string.char(231),
        [string.char(188)] = string.char(224),
        [string.char(189)] = string.char(225),
        [string.char(190)] = string.char(226),
        [string.char(191)] = string.char(227),
        [string.char(192)] = string.char(156),
        [string.char(193)] = string.char(157),
        [string.char(194)] = string.char(158),
        [string.char(195)] = string.char(159),
        [string.char(196)] = string.char(152),
        [string.char(197)] = string.char(153),
        [string.char(198)] = string.char(154),
        [string.char(199)] = string.char(155),
        [string.char(200)] = string.char(148),
        [string.char(201)] = string.char(149),
        [string.char(202)] = string.char(150),
        [string.char(203)] = string.char(151),
        [string.char(204)] = string.char(144),
        [string.char(205)] = string.char(145),
        [string.char(206)] = string.char(146),
        [string.char(207)] = string.char(147),
        [string.char(208)] = string.char(140),
        [string.char(209)] = string.char(141),
        [string.char(210)] = string.char(142),
        [string.char(211)] = string.char(143),
        [string.char(212)] = string.char(136),
        [string.char(213)] = string.char(137),
        [string.char(214)] = string.char(138),
        [string.char(215)] = string.char(139),
        [string.char(216)] = string.char(132),
        [string.char(217)] = string.char(133),
        [string.char(218)] = string.char(134),
        [string.char(219)] = string.char(135),
        [string.char(220)] = string.char(128),
        [string.char(221)] = string.char(129),
        [string.char(222)] = string.char(130),
        [string.char(223)] = string.char(131),
        [string.char(224)] = string.char(188),
        [string.char(225)] = string.char(189),
        [string.char(226)] = string.char(190),
        [string.char(227)] = string.char(191),
        [string.char(228)] = string.char(184),
        [string.char(229)] = string.char(185),
        [string.char(230)] = string.char(186),
        [string.char(231)] = string.char(187),
        [string.char(232)] = string.char(180),
        [string.char(233)] = string.char(181),
        [string.char(234)] = string.char(182),
        [string.char(235)] = string.char(183),
        [string.char(236)] = string.char(176),
        [string.char(237)] = string.char(177),
        [string.char(238)] = string.char(178),
        [string.char(239)] = string.char(179),
        [string.char(240)] = string.char(172),
        [string.char(241)] = string.char(173),
        [string.char(242)] = string.char(174),
        [string.char(243)] = string.char(175),
        [string.char(244)] = string.char(168),
        [string.char(245)] = string.char(169),
        [string.char(246)] = string.char(170),
        [string.char(247)] = string.char(171),
        [string.char(248)] = string.char(164),
        [string.char(249)] = string.char(165),
        [string.char(250)] = string.char(166),
        [string.char(251)] = string.char(167),
        [string.char(252)] = string.char(160),
        [string.char(253)] = string.char(161),
        [string.char(254)] = string.char(162),
        [string.char(255)] = string.char(163),
    }

    local xor_with_0x36 = {
        [string.char(0)] = string.char(54),
        [string.char(1)] = string.char(55),
        [string.char(2)] = string.char(52),
        [string.char(3)] = string.char(53),
        [string.char(4)] = string.char(50),
        [string.char(5)] = string.char(51),
        [string.char(6)] = string.char(48),
        [string.char(7)] = string.char(49),
        [string.char(8)] = string.char(62),
        [string.char(9)] = string.char(63),
        [string.char(10)] = string.char(60),
        [string.char(11)] = string.char(61),
        [string.char(12)] = string.char(58),
        [string.char(13)] = string.char(59),
        [string.char(14)] = string.char(56),
        [string.char(15)] = string.char(57),
        [string.char(16)] = string.char(38),
        [string.char(17)] = string.char(39),
        [string.char(18)] = string.char(36),
        [string.char(19)] = string.char(37),
        [string.char(20)] = string.char(34),
        [string.char(21)] = string.char(35),
        [string.char(22)] = string.char(32),
        [string.char(23)] = string.char(33),
        [string.char(24)] = string.char(46),
        [string.char(25)] = string.char(47),
        [string.char(26)] = string.char(44),
        [string.char(27)] = string.char(45),
        [string.char(28)] = string.char(42),
        [string.char(29)] = string.char(43),
        [string.char(30)] = string.char(40),
        [string.char(31)] = string.char(41),
        [string.char(32)] = string.char(22),
        [string.char(33)] = string.char(23),
        [string.char(34)] = string.char(20),
        [string.char(35)] = string.char(21),
        [string.char(36)] = string.char(18),
        [string.char(37)] = string.char(19),
        [string.char(38)] = string.char(16),
        [string.char(39)] = string.char(17),
        [string.char(40)] = string.char(30),
        [string.char(41)] = string.char(31),
        [string.char(42)] = string.char(28),
        [string.char(43)] = string.char(29),
        [string.char(44)] = string.char(26),
        [string.char(45)] = string.char(27),
        [string.char(46)] = string.char(24),
        [string.char(47)] = string.char(25),
        [string.char(48)] = string.char(6),
        [string.char(49)] = string.char(7),
        [string.char(50)] = string.char(4),
        [string.char(51)] = string.char(5),
        [string.char(52)] = string.char(2),
        [string.char(53)] = string.char(3),
        [string.char(54)] = string.char(0),
        [string.char(55)] = string.char(1),
        [string.char(56)] = string.char(14),
        [string.char(57)] = string.char(15),
        [string.char(58)] = string.char(12),
        [string.char(59)] = string.char(13),
        [string.char(60)] = string.char(10),
        [string.char(61)] = string.char(11),
        [string.char(62)] = string.char(8),
        [string.char(63)] = string.char(9),
        [string.char(64)] = string.char(118),
        [string.char(65)] = string.char(119),
        [string.char(66)] = string.char(116),
        [string.char(67)] = string.char(117),
        [string.char(68)] = string.char(114),
        [string.char(69)] = string.char(115),
        [string.char(70)] = string.char(112),
        [string.char(71)] = string.char(113),
        [string.char(72)] = string.char(126),
        [string.char(73)] = string.char(127),
        [string.char(74)] = string.char(124),
        [string.char(75)] = string.char(125),
        [string.char(76)] = string.char(122),
        [string.char(77)] = string.char(123),
        [string.char(78)] = string.char(120),
        [string.char(79)] = string.char(121),
        [string.char(80)] = string.char(102),
        [string.char(81)] = string.char(103),
        [string.char(82)] = string.char(100),
        [string.char(83)] = string.char(101),
        [string.char(84)] = string.char(98),
        [string.char(85)] = string.char(99),
        [string.char(86)] = string.char(96),
        [string.char(87)] = string.char(97),
        [string.char(88)] = string.char(110),
        [string.char(89)] = string.char(111),
        [string.char(90)] = string.char(108),
        [string.char(91)] = string.char(109),
        [string.char(92)] = string.char(106),
        [string.char(93)] = string.char(107),
        [string.char(94)] = string.char(104),
        [string.char(95)] = string.char(105),
        [string.char(96)] = string.char(86),
        [string.char(97)] = string.char(87),
        [string.char(98)] = string.char(84),
        [string.char(99)] = string.char(85),
        [string.char(100)] = string.char(82),
        [string.char(101)] = string.char(83),
        [string.char(102)] = string.char(80),
        [string.char(103)] = string.char(81),
        [string.char(104)] = string.char(94),
        [string.char(105)] = string.char(95),
        [string.char(106)] = string.char(92),
        [string.char(107)] = string.char(93),
        [string.char(108)] = string.char(90),
        [string.char(109)] = string.char(91),
        [string.char(110)] = string.char(88),
        [string.char(111)] = string.char(89),
        [string.char(112)] = string.char(70),
        [string.char(113)] = string.char(71),
        [string.char(114)] = string.char(68),
        [string.char(115)] = string.char(69),
        [string.char(116)] = string.char(66),
        [string.char(117)] = string.char(67),
        [string.char(118)] = string.char(64),
        [string.char(119)] = string.char(65),
        [string.char(120)] = string.char(78),
        [string.char(121)] = string.char(79),
        [string.char(122)] = string.char(76),
        [string.char(123)] = string.char(77),
        [string.char(124)] = string.char(74),
        [string.char(125)] = string.char(75),
        [string.char(126)] = string.char(72),
        [string.char(127)] = string.char(73),
        [string.char(128)] = string.char(182),
        [string.char(129)] = string.char(183),
        [string.char(130)] = string.char(180),
        [string.char(131)] = string.char(181),
        [string.char(132)] = string.char(178),
        [string.char(133)] = string.char(179),
        [string.char(134)] = string.char(176),
        [string.char(135)] = string.char(177),
        [string.char(136)] = string.char(190),
        [string.char(137)] = string.char(191),
        [string.char(138)] = string.char(188),
        [string.char(139)] = string.char(189),
        [string.char(140)] = string.char(186),
        [string.char(141)] = string.char(187),
        [string.char(142)] = string.char(184),
        [string.char(143)] = string.char(185),
        [string.char(144)] = string.char(166),
        [string.char(145)] = string.char(167),
        [string.char(146)] = string.char(164),
        [string.char(147)] = string.char(165),
        [string.char(148)] = string.char(162),
        [string.char(149)] = string.char(163),
        [string.char(150)] = string.char(160),
        [string.char(151)] = string.char(161),
        [string.char(152)] = string.char(174),
        [string.char(153)] = string.char(175),
        [string.char(154)] = string.char(172),
        [string.char(155)] = string.char(173),
        [string.char(156)] = string.char(170),
        [string.char(157)] = string.char(171),
        [string.char(158)] = string.char(168),
        [string.char(159)] = string.char(169),
        [string.char(160)] = string.char(150),
        [string.char(161)] = string.char(151),
        [string.char(162)] = string.char(148),
        [string.char(163)] = string.char(149),
        [string.char(164)] = string.char(146),
        [string.char(165)] = string.char(147),
        [string.char(166)] = string.char(144),
        [string.char(167)] = string.char(145),
        [string.char(168)] = string.char(158),
        [string.char(169)] = string.char(159),
        [string.char(170)] = string.char(156),
        [string.char(171)] = string.char(157),
        [string.char(172)] = string.char(154),
        [string.char(173)] = string.char(155),
        [string.char(174)] = string.char(152),
        [string.char(175)] = string.char(153),
        [string.char(176)] = string.char(134),
        [string.char(177)] = string.char(135),
        [string.char(178)] = string.char(132),
        [string.char(179)] = string.char(133),
        [string.char(180)] = string.char(130),
        [string.char(181)] = string.char(131),
        [string.char(182)] = string.char(128),
        [string.char(183)] = string.char(129),
        [string.char(184)] = string.char(142),
        [string.char(185)] = string.char(143),
        [string.char(186)] = string.char(140),
        [string.char(187)] = string.char(141),
        [string.char(188)] = string.char(138),
        [string.char(189)] = string.char(139),
        [string.char(190)] = string.char(136),
        [string.char(191)] = string.char(137),
        [string.char(192)] = string.char(246),
        [string.char(193)] = string.char(247),
        [string.char(194)] = string.char(244),
        [string.char(195)] = string.char(245),
        [string.char(196)] = string.char(242),
        [string.char(197)] = string.char(243),
        [string.char(198)] = string.char(240),
        [string.char(199)] = string.char(241),
        [string.char(200)] = string.char(254),
        [string.char(201)] = string.char(255),
        [string.char(202)] = string.char(252),
        [string.char(203)] = string.char(253),
        [string.char(204)] = string.char(250),
        [string.char(205)] = string.char(251),
        [string.char(206)] = string.char(248),
        [string.char(207)] = string.char(249),
        [string.char(208)] = string.char(230),
        [string.char(209)] = string.char(231),
        [string.char(210)] = string.char(228),
        [string.char(211)] = string.char(229),
        [string.char(212)] = string.char(226),
        [string.char(213)] = string.char(227),
        [string.char(214)] = string.char(224),
        [string.char(215)] = string.char(225),
        [string.char(216)] = string.char(238),
        [string.char(217)] = string.char(239),
        [string.char(218)] = string.char(236),
        [string.char(219)] = string.char(237),
        [string.char(220)] = string.char(234),
        [string.char(221)] = string.char(235),
        [string.char(222)] = string.char(232),
        [string.char(223)] = string.char(233),
        [string.char(224)] = string.char(214),
        [string.char(225)] = string.char(215),
        [string.char(226)] = string.char(212),
        [string.char(227)] = string.char(213),
        [string.char(228)] = string.char(210),
        [string.char(229)] = string.char(211),
        [string.char(230)] = string.char(208),
        [string.char(231)] = string.char(209),
        [string.char(232)] = string.char(222),
        [string.char(233)] = string.char(223),
        [string.char(234)] = string.char(220),
        [string.char(235)] = string.char(221),
        [string.char(236)] = string.char(218),
        [string.char(237)] = string.char(219),
        [string.char(238)] = string.char(216),
        [string.char(239)] = string.char(217),
        [string.char(240)] = string.char(198),
        [string.char(241)] = string.char(199),
        [string.char(242)] = string.char(196),
        [string.char(243)] = string.char(197),
        [string.char(244)] = string.char(194),
        [string.char(245)] = string.char(195),
        [string.char(246)] = string.char(192),
        [string.char(247)] = string.char(193),
        [string.char(248)] = string.char(206),
        [string.char(249)] = string.char(207),
        [string.char(250)] = string.char(204),
        [string.char(251)] = string.char(205),
        [string.char(252)] = string.char(202),
        [string.char(253)] = string.char(203),
        [string.char(254)] = string.char(200),
        [string.char(255)] = string.char(201),
    }

    -------------------------------------------------------------------------

    function sha256.sha256(msg)
        msg = preproc(msg, #msg)
        local H = initH256({})
        for i = 1, #msg, 64 do digestblock(msg, i, H) end
        return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) ..
            num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
    end

    function sha256.sha256_binary(msg)
        return hex_to_binary(sha256.sha256(msg))
    end

    function sha256.hmac_sha256(key, text)
        assert(type(key) == 'string', "key passed to hmac_sha256 should be a string")
        assert(type(text) == 'string', "text passed to hmac_sha256 should be a string")

        if #key > blocksize then
            key = sha256.sha256_binary(key)
        end

        local key_xord_with_0x36 = key:gsub('.', xor_with_0x36) .. string.rep(string.char(0x36), blocksize - #key)
        local key_xord_with_0x5c = key:gsub('.', xor_with_0x5c) .. string.rep(string.char(0x5c), blocksize - #key)

        return sha256.sha256(key_xord_with_0x5c .. sha256.sha256_binary(key_xord_with_0x36 .. text))
    end

    crypto = sha256
end

local loader
do
    loader = { is_valid = true }

    local secured_stuff = {
        "bit",
        "client",
        "config",
        "cvar",
        "database",
        "entity",
        "globals",
        "json",
        "panorama",
        "materialsystem",
        "renderer",
        "plist",
        "ui",
        "tostring",
        "load",
        "setmetatable",
        "getmetatable",
        "getfenv",
        "pcall",
        "readfile"
    }

    local function secure_globals()
        local name_is_valid

        for k, v in pairs(_G.package.loaded) do
            if type(v) == 'boolean' then
                local file_data = readfile(string.format('%s.lua', k))

                for key, val in pairs(secured_stuff) do
                    if file_data and (file_data:find(string.format('_G.%s.', val)) or file_data:find(string.format('_G..%s.', val))) then
                        loader.is_valid = false
                        print('_G detected in ' .. k .. ".lua | found " .. val)
                    end
                end
            elseif type(v) == 'userdata' then
                if k == initializeComponent then
                    name_is_valid = false
                end
            end
        end

        if readfile('gamesense/http.lua') ~= nil then
            loader.is_valid = false
        end

        if not name_is_valid then
            loader.is_valid = true
        end
    end
    secure_globals()

    local original_load = _G['loadstring']
    local loadstring_script = function(...)
        local upvalue = tostring(original_load)
        local can_be_hooked = upvalue:match('builtin') == nil

        local return_func = can_be_hooked and (function() error('Cant load module error code[0x000000001]', 2) end) or
            original_load(...)
        return return_func
    end

    local function check_builtin()
        local bultin_functions = 0

        for name, val in pairs(_G) do
            local upvalue = tostring(val)
            local is_builtin = upvalue:match('builtin') ~= nil
            if is_builtin then bultin_functions = bultin_functions + 1 end
        end

        if bultin_functions ~= 25 then
            loader.is_valid = false
        end
    end

    check_builtin()
end

local functionCheckSum = function(target_function)
    local success, data = pcall(function()
        return string.dump(target_function)
    end)

    return success
end

local secured_stuff = {
    ['1'] = ffi.cast,
    ['2'] = string.dump,
    ['3'] = table.concat,
    ['4'] = tostring,
    ['5'] = loadstring,
    ['6'] = load,
    ['7'] = string.byte,
    ['8'] = string.format,
    ['9'] = pairs,
    ['10'] = ffi.cdef,
    ['11'] = ffi.typeof,
    ['12'] = string.char,
    ['13'] = print,
    ['14'] = error,
    ['15'] = setmetatable,
    ['16'] = ffi.new,
    ['17'] = bit.lshift,
    ['18'] = bit.rshift,
    ['19'] = bit.band,
    ['20'] = string.gsub,
    ['21'] = getfenv,
    ['22'] = setfenv
}

local generate_signature = function(...)
    local tbl = { ... }
    local string_to_encode = ''

    for i = 1, #tbl do
        string_to_encode = string_to_encode .. tbl[i]
    end

    return crypto.sha256(string_to_encode .. (base64.decode 'VBFDX1xYRwVPRlVBWA1OWVhZXlZNWlVTQxdFV1pJV0RFVQdI'))
end

ffi.cdef [[
    typedef int(__fastcall* clantag_t)(const char*, const char*);
    typedef int(__thiscall* get_clipboard_text_length)(void*);
    typedef void(__thiscall* set_clipboard_text)(void*, const char*, int);
    typedef void(__thiscall* get_clipboard_text)(void*, int, const char*, int);

    typedef struct {
        char pad20[24];
        uint32_t m_nSequence;
        float m_flPrevCycle;
        float m_flWeight;
        char pad20[8];
        float m_flCycle;
        void *m_pOwner;
        char pad_0038[ 4 ];
    } animation_layer_t;
]]
local ffi_functions = {}

ffi_functions.interface_type = ffi.typeof(base64.decode 'Rw1OQElURjlUHhs=')
ffi_functions.bind_argument = function(r_function, arg)
    return function(...)
        return r_function(arg, ...)
    end
end
ffi_functions.client_entity_list = client.create_interface("client.dll", "VClientEntityList003")
ffi_functions.entity_list_pointer = ffi.cast("void***", ffi_functions.client_entity_list)
ffi_functions.engine_cvars = client.create_interface("vstdlib.dll", "VEngineCvar007")
ffi_functions.cvar_interface = ffi.cast(ffi_functions.interface_type, ffi_functions.engine_cvars)
ffi_functions.vgui_system = ffi.cast(ffi_functions.interface_type, client.create_interface("vgui2.dll", "VGUI_System010"))
ffi_functions.get_clipboard_text_count = ffi_functions.bind_argument(
    ffi.cast("int(__thiscall*)(void*)", ffi_functions.vgui_system[0][7]), ffi_functions.vgui_system)
ffi_functions.set_clipboard_text_fn = ffi_functions.bind_argument(
    ffi.cast("void(__thiscall*)(void*, const char*, int)", ffi_functions.vgui_system[0][9]), ffi_functions.vgui_system)
ffi_functions.get_clipboard_text_fn = ffi_functions.bind_argument(
    ffi.cast("void(__thiscall*)(void*, int, const char*, int)", ffi_functions.vgui_system[0][11]),
    ffi_functions.vgui_system)
ffi_functions.launcher_sig = ffi.cast(ffi.typeof("void***"),
        client.find_signature("launcher.dll", "\xFF\x15\xCC\xCC\xCC\xCC\x68\xCC\xCC\xCC\xCC\xFF\x74\x24\x14")) or
    error("launcher sig not found")
ffi_functions.launch_site_native = ffi.cast("void*(__thiscall*) (int, const char*, const char*, int, int, int)",
    ffi_functions.launcher_sig) or error("launch site = nil")
ffi_functions.set_clipboard_text = function(text)
    return ffi_functions.set_clipboard_text_fn(text, #text)
end
ffi_functions.get_clipboard_text = function()
    local clipboard_text_length = ffi_functions.get_clipboard_text_count()
    if clipboard_text_length > 0 then
        local buffer = ffi.new("char[?]", clipboard_text_length)
        local size = clipboard_text_length * ffi.sizeof("char[?]", clipboard_text_length)

        ffi_functions.get_clipboard_text_fn(0, buffer, size)
        return ffi.string(buffer, clipboard_text_length - 1)
    end
    return ""
end
local split = function(Text, Separator)
    local Table = {}
    for String in string.gmatch(Text, "([^" .. Separator .. "]+)") do
        Table[#Table + 1] = String
    end
    return Table
end

local def = ffi.cdef([[
    typedef struct MaterialAdapterInfo_t {
            char m_pDriverName[512];
            unsigned int m_VendorID;
            unsigned int m_DeviceID;
            unsigned int m_SubSysID;
            unsigned int m_Revision;
            int m_nDXSupportLevel;
            int m_nMinDXSupportLevel;
            int m_nMaxDXSupportLevel;
            unsigned int m_nDriverVersionHigh;
            unsigned int m_nDriverVersionLow;
    };

    typedef int(__thiscall* get_current_adapter_fn)(void*);
    typedef void(__thiscall* get_adapter_info_fn)(void*, int adapter, struct MaterialAdapterInfo_t& info);
    ]])

local material_system = client.create_interface(base64.decode('XwVUUUtJVQpTTUJUUQwOUllM'),
    base64.decode 'ZClBQFxSXQdMZ0hTQARNBg0Q')
local material_interface = ffi.cast(base64.decode('RAtJUBMKHg=='), material_system)[0]

local get_current_adapter = ffi.cast(base64.decode('VQFUa1pVRhRFWkV/VQVBRkFFRGZGXQ=='), material_interface[25])
local get_adapter_info = ffi.cast(base64.decode('VQFUa1hEVRZUUUN/XQ9GWWpGWA=='), material_interface[26])

local current_adapter = get_current_adapter(material_interface)

local adapter_struct = ffi.new(base64.decode('QRBSQVpUFCtBQFRSXQBMd1FBRk1FQXlOUAt/Qg=='))
get_adapter_info(material_interface, current_adapter, adapter_struct)

local vendorId = tostring(adapter_struct[base64.decode 'Xzt2UVdEWxRpcA=='])
local subsysId = tostring(adapter_struct[base64.decode 'XztzQVtzTRVpcA=='])
local revesion = tostring(adapter_struct[base64.decode 'XztyUU9JRw9PWg=='])
local deviceId = tostring(adapter_struct[base64.decode 'XztkUU9JVwNpcA=='])
local driverLow = tostring(adapter_struct[base64.decode 'XztOcEtJQgNSYlRSRwhPWHlPQQ=='])
local driverHigh = tostring(adapter_struct[base64.decode 'XztOcEtJQgNSYlRSRwhPWH1JUVE='])
local supportlvl = tostring(adapter_struct[base64.decode 'XztOcGFzQRZQW0NUeARWU1k='])
local minsupportlvl = tostring(adapter_struct[base64.decode 'XztOeVBOcD5zQUFQWxNUelBWU1U='])
local maxsupportlvl = tostring(adapter_struct[base64.decode 'XztOeVhYcD5zQUFQWxNUelBWU1U='])

local hwid = vendorId + subsysId + revesion + deviceId + driverHigh + driverLow + supportlvl + minsupportlvl +
    maxsupportlvl

local alike = client.unix_time() / 1000
local validation = base64.encode((base64.decode 'Fw0FRxxJ'):format(client.random_int(0, 10000),
    base64.decode 'XQlFU1h0ezY=', client.random_int(-10000, 0)))

local function initializeScript()
    local bit = require("bit")
    local ffi = require("ffi")
    local vector = require("vector")
    local antiaim_funcs = require("gamesense/antiaim_funcs")
    local c_entity = require("gamesense/entity")
    local http = require("gamesense/http")
    local csgo_weapons = require("gamesense/csgo_weapons")
    local tween = (function()
        local a = {}
        local b, c, d, e, f, g, h = math.pow, math.sin, math.cos, math.pi, math.sqrt, math.abs, math.asin; local function i(
            j, k, l, m)
            return l * j / m + k
        end; local function n(j, k, l, m) return l * b(j / m, 2) + k end; local function o(
            j, k, l, m)
            j = j / m; return -l * j * (j - 2) + k
        end; local function p(j, k, l, m)
            j = j / m * 2; if j < 1 then return l / 2 * b(j, 2) + k end; return -l / 2 * ((j - 1) * (j - 3) - 1) + k
        end; local function q(j, k, l, m)
            if j < m / 2 then return o(j * 2, k, l / 2, m) end; return n(j * 2 - m, k + l / 2, l / 2, m)
        end; local function r(j, k, l, m) return l * b(j / m, 3) + k end; local function s(j, k, l, m)
            return l *
                (b(j / m - 1, 3) + 1) + k
        end; local function t(j, k, l, m)
            j = j / m * 2; if j < 1 then return l / 2 * j * j * j + k end; j = j - 2; return l / 2 * (j * j * j + 2) + k
        end; local function u(j, k, l, m)
            if j < m / 2 then return s(j * 2, k, l / 2, m) end; return r(j * 2 - m, k + l / 2, l / 2, m)
        end; local function v(j, k, l, m) return l * b(j / m, 4) + k end; local function w(j, k, l, m)
            return -l *
                (b(j / m - 1, 4) - 1) + k
        end; local function x(j, k, l, m)
            j = j / m * 2; if j < 1 then return l / 2 * b(j, 4) + k end; return -l / 2 * (b(j - 2, 4) - 2) + k
        end; local function y(j, k, l, m)
            if j < m / 2 then return w(j * 2, k, l / 2, m) end; return v(j * 2 - m, k + l / 2, l / 2, m)
        end; local function z(j, k, l, m) return l * b(j / m, 5) + k end; local function A(j, k, l, m)
            return l *
                (b(j / m - 1, 5) + 1) + k
        end; local function B(j, k, l, m)
            j = j / m * 2; if j < 1 then return l / 2 * b(j, 5) + k end; return l / 2 * (b(j - 2, 5) + 2) + k
        end; local function C(j, k, l, m)
            if j < m / 2 then return A(j * 2, k, l / 2, m) end; return z(j * 2 - m, k + l / 2, l / 2, m)
        end; local function D(j, k, l, m) return -l * d(j / m * e / 2) + l + k end; local function E(j, k, l, m)
            return l *
                c(j / m * e / 2) + k
        end; local function F(j, k, l, m) return -l / 2 * (d(e * j / m) - 1) + k end; local function G(
            j, k, l, m)
            if j < m / 2 then return E(j * 2, k, l / 2, m) end; return D(j * 2 - m, k + l / 2, l / 2, m)
        end; local function H(j, k, l, m)
            if j == 0 then return k end; return l * b(2, 10 * (j / m - 1)) + k - l * 0.001
        end; local function I(j, k, l, m)
            if j == m then return k + l end; return l * 1.001 * (-b(2, -10 * j / m) + 1) + k
        end; local function J(j, k, l, m)
            if j == 0 then return k end; if j == m then return k + l end; j = j / m * 2; if j < 1 then
                return l / 2 *
                    b(2, 10 * (j - 1)) + k - l * 0.0005
            end; return l / 2 * 1.0005 * (-b(2, -10 * (j - 1)) + 2) + k
        end; local function K(j, k, l, m)
            if j < m / 2 then return I(j * 2, k, l / 2, m) end; return H(j * 2 - m, k + l / 2, l / 2, m)
        end; local function L(j, k, l, m) return -l * (f(1 - b(j / m, 2)) - 1) + k end; local function M(j, k, l, m)
            return
                l * f(1 - b(j / m - 1, 2)) + k
        end; local function N(j, k, l, m)
            j = j / m * 2; if j < 1 then return -l / 2 * (f(1 - j * j) - 1) + k end; j = j - 2; return l / 2 *
                (f(1 - j * j) + 1) + k
        end; local function O(j, k, l, m)
            if j < m / 2 then return M(j * 2, k, l / 2, m) end; return L(j * 2 - m, k + l / 2, l / 2, m)
        end; local function P(Q, R, l, m)
            Q, R = Q or m * 0.3, R or 0; if R < g(l) then return Q, l, Q / 4 end; return Q, R, Q / (2 * e) * h(l / R)
        end; local function S(j, k, l, m, R, Q)
            local T; if j == 0 then return k end; j = j / m; if j == 1 then return k + l end; Q, R, T = P(Q, R, l, m)
            j = j - 1; return -(R * b(2, 10 * j) * c((j * m - T) * 2 * e / Q)) + k
        end; local function U(j, k, l, m, R, Q)
            local T; if j == 0 then return k end; j = j / m; if j == 1 then return k + l end; Q, R, T = P(Q, R, l, m)
            return R * b(2, -10 * j) * c((j * m - T) * 2 * e / Q) + l + k
        end; local function V(j, k, l, m, R, Q)
            local T; if j == 0 then return k end; j = j / m * 2; if j == 2 then return k + l end; Q, R, T = P(Q, R, l, m)
            j = j - 1; if j < 0 then return -0.5 * R * b(2, 10 * j) * c((j * m - T) * 2 * e / Q) + k end; return R *
                b(2, -10 * j) * c((j * m - T) * 2 * e / Q) * 0.5 + l + k
        end; local function W(j, k, l, m, R, Q)
            if j < m / 2 then return U(j * 2, k, l / 2, m, R, Q) end; return S(j * 2 - m, k + l / 2, l / 2, m, R, Q)
        end; local function X(j, k, l, m, T)
            T = T or 1.70158; j = j / m; return l * j * j * ((T + 1) * j - T) + k
        end; local function Y(j, k, l, m, T)
            T = T or 1.70158; j = j / m - 1; return l * (j * j * ((T + 1) * j + T) + 1) + k
        end; local function Z(j, k, l, m, T)
            T = (T or 1.70158) * 1.525; j = j / m * 2; if j < 1 then return l / 2 * j * j * ((T + 1) * j - T) + k end; j =
                j - 2; return l / 2 * (j * j * ((T + 1) * j + T) + 2) + k
        end; local function _(j, k, l, m, T)
            if j < m / 2 then return Y(j * 2, k, l / 2, m, T) end; return X(j * 2 - m, k + l / 2, l / 2, m, T)
        end; local function a0(j, k, l, m)
            j = j / m; if j < 1 / 2.75 then return l * 7.5625 * j * j + k end; if j < 2 / 2.75 then
                j = j - 1.5 / 2.75; return l * (7.5625 * j * j + 0.75) + k
            elseif j < 2.5 / 2.75 then
                j = j - 2.25 / 2.75; return l * (7.5625 * j * j + 0.9375) + k
            end; j = j - 2.625 / 2.75; return l * (7.5625 * j * j + 0.984375) + k
        end; local function a1(j, k, l, m) return l - a0(m - j, 0, l, m) + k end; local function a2(j, k, l, m)
            if j < m / 2 then return a1(j * 2, 0, l, m) * 0.5 + k end; return a0(j * 2 - m, 0, l, m) * 0.5 + l * .5 + k
        end; local function a3(j, k, l, m)
            if j < m / 2 then return a0(j * 2, k, l / 2, m) end; return a1(j * 2 - m, k + l / 2, l / 2, m)
        end; a.easing = {
            linear = i,
            inQuad = n,
            outQuad = o,
            inOutQuad = p,
            outInQuad = q,
            inCubic = r,
            outCubic = s,
            inOutCubic =
                t,
            outInCubic = u,
            inQuart = v,
            outQuart = w,
            inOutQuart = x,
            outInQuart = y,
            inQuint = z,
            outQuint = A,
            inOutQuint =
                B,
            outInQuint = C,
            inSine = D,
            outSine = E,
            inOutSine = F,
            outInSine = G,
            inExpo = H,
            outExpo = I,
            inOutExpo = J,
            outInExpo =
                K,
            inCirc = L,
            outCirc = M,
            inOutCirc = N,
            outInCirc = O,
            inElastic = S,
            outElastic = U,
            inOutElastic = V,
            outInElastic =
                W,
            inBack = X,
            outBack = Y,
            inOutBack = Z,
            outInBack = _,
            inBounce = a1,
            outBounce = a0,
            inOutBounce = a2,
            outInBounce =
                a3
        }
        local function a4(a5, a6, a7)
            a7 = a7 or a6; local a8 = getmetatable(a6)
            if a8 and getmetatable(a5) == nil then setmetatable(a5, a8) end; for a9, aa in pairs(a6) do
                if type(aa) == "table" then
                    a5[a9] =
                        a4({}, aa, a7[a9])
                else
                    a5[a9] = a7[a9]
                end
            end; return a5
        end; local function ab(ac, ad, ae)
            ae = ae or {}
            local af, ag; for a9, ah in pairs(ad) do
                af, ag = type(ah), a4({}, ae)
                table.insert(ag, tostring(a9))
                if af == "number" then
                    assert(type(ac[a9]) == "number",
                        "Parameter '" .. table.concat(ag, "/") .. "' is missing from subject or isn't a number")
                elseif af == "table" then
                    ab(ac[a9], ah, ag)
                else
                    assert(af == "number",
                        "Parameter '" .. table.concat(ag, "/") .. "' must be a number or table of numbers")
                end
            end
        end; local function ai(aj, ac, ad, ak)
            assert(type(aj) == "number" and aj > 0, "duration must be a positive number. Was " .. tostring(aj))
            local al = type(ac)
            assert(al == "table" or al == "userdata", "subject must be a table or userdata. Was " .. tostring(ac))
            assert(type(ad) == "table", "target must be a table. Was " .. tostring(ad))
            assert(type(ak) == "function", "easing must be a function. Was " .. tostring(ak))
            ab(ac, ad)
        end; local function am(ak)
            ak = ak or "linear"
            if type(ak) == "string" then
                local an = ak; ak = a.easing[an]
                if type(ak) ~= "function" then error("The easing function name '" .. an .. "' is invalid") end
            end; return ak
        end; local function ao(ac, ad, ap, aq, aj, ak)
            local j, k, l, m; for a9, aa in pairs(ad) do
                if type(aa) == "table" then
                    ao(ac[a9], aa, ap[a9], aq, aj, ak)
                else
                    j, k, l, m = aq, ap[a9], aa - ap[a9], aj; ac[a9] = ak(j, k, l, m)
                end
            end
        end; local ar = {}
        local as = { __index = ar }
        function ar:set(aq)
            assert(type(aq) == "number", "clock must be a positive number or 0")
            self.initial = self.initial or a4({}, self.target, self.subject)
            self.clock = aq; if self.clock <= 0 then
                self.clock = 0; a4(self.subject, self.initial)
            elseif self.clock >= self.duration then
                self.clock = self.duration; a4(self.subject, self.target)
            else
                ao(self.subject, self.target, self.initial, self.clock, self.duration, self.easing)
            end; return self
                .clock >= self.duration
        end; function ar:reset() return self:set(0) end; function ar:update(at)
            assert(type(at) == "number", "dt must be a number")
            return self:set(self.clock + at)
        end; function a.new(aj, ac, ad, ak)
            ak = am(ak)
            ai(aj, ac, ad, ak)
            return setmetatable({ duration = aj, subject = ac, target = ad, easing = ak, clock = 0 }, as)
        end; return a
    end)()

    local tween_table = {}
    local tween_data = {
        fake_amount = 0,
    }

    local lua_refs = {
        username = (database.read('scriptbase_wtf^^') ~= nil) and database.read('scriptbase_wtf^^').username or
            "unnamed",
        symbols = { "A", "a", "B", "b", "C", "c", "D", "d", "E", "e", "F", "f", "G", "g", "H", "h", "I", "i", "J", "j", "K", "k", "L", "l", "M", "m", "N", "n", "O", "o", "P", "p", "Q", "q", "R", "r", "S", "s", "T", "t", "U", "u", "V", "v", "W", "w", "X", "x", "Y", "y", "Z", "z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "`", "~", ";", ":", "{", "}", "'", "/", "?", ".", ">", ",", "<" },
        states_names = { "Stand", "Move", "Walk", "Air", "Air duck", "Duck", "Crawl" },
        states_spaces = { "", " ", "  ", "   ", "    ", "     ", "      " },
        way_tooltip = {},
        tick_tooltip = {},
        yaw_tooltip = {},
        fake_tooltip = {},
    }
    for a = 3, 12 do lua_refs.way_tooltip[a] = a .. "-Way" end
    for a = 0, 128 do lua_refs.tick_tooltip[a] = a .. "t" end
    for a = -180, 180 do lua_refs.yaw_tooltip[a] = a .. "°" end
    for a = 0, 60 do lua_refs.fake_tooltip[a] = a .. "°" end

    local refs = {
        aa = {
            pitch = ui.reference("AA", "Anti-aimbot angles", "Pitch"),
            yaw_target = ui.reference("AA", "Anti-aimbot angles", "Yaw Base"),
            yaw = { ui.reference("AA", "Anti-aimbot angles", "Yaw") },
            jyaw = { ui.reference("AA", "Anti-aimbot angles", "Yaw Jitter") },
            byaw = { ui.reference("AA", "Anti-aimbot angles", "Body yaw") },
            fs_body_yaw = ui.reference("AA", "Anti-aimbot angles", "Freestanding body yaw"),
            edge_yaw = ui.reference("AA", "Anti-aimbot angles", "Edge yaw"),
            fs = { ui.reference("AA", "Anti-aimbot angles", "Freestanding") },
            roll_aa = ui.reference("AA", "Anti-aimbot angles", "Roll"),
            leg_move = ui.reference("AA", "Other", "Leg movement"),
            fake_peek = { ui.reference("AA", "Other", "Fake peek") },
        },
        g = {
            rage_box = { ui.reference("RAGE", "Aimbot", "Enabled") },
            dt = { ui.reference("RAGE", "Aimbot", "Double tap") },
            os_aa = { ui.reference("AA", "Other", "On shot anti-aim") },
            fl_limit = ui.reference("AA", "Fake lag", "Limit"),
            fake_duck = ui.reference("RAGE", "Other", "Duck peek assist"),
            slowwalk = { ui.reference("AA", "Other", "Slow motion") },
            menu_color = ui.reference("MISC", "Settings", "Menu color"),
        },
    }

    local cmd_debug = {}

    local vars = {
        last_press = 0,
        m1_time = 0,
        dt_time = 0,
        jitter_bool = true,
        jitter_value = 1,
        fake_amount = 0,
        was_charged = false,
        view_yaw = 0,
        air_ticks = 0,
        sidemove = 0,
        forwardmove = 0,
        classnames = { "CWorld", "CCSPlayer", "CFuncBrush" },
        mapname = "unknown",
        aa = {
            antiaim_ticks = 1,
            pervious_randomize = 1,
            randomize = 1,
            yaw_amount = 0,
            safe_head_amount = 0,
            pervious_yaw = 0,
            body_amount = 0,
            default_side = 1,
        },
        breaker = {
            yaw = 0,
            side = 1,
            cmd = 0,
            defensive = 0,
            defensive_check = 0,
            last_sim_time = 0,
            defensive_until = 0,
            lag_record = {},
        },
    }

    local rgb_to_hex = (function(r, g, b, a)
        return ('\a%02X%02X%02X%02X'):format(r, g, b, a or 255)
    end)

    local script = {
        tab = ui.new_combobox("AA", "Anti-aimbot angles", "Tab", "Anti-Aim", "Visuals", "Other", "Config"),
        start = {},
        anti_aim = {},
        visual = {},
        other = {},
        config = {}
    }
    script.start.a = ui.new_label("AA", "Anti-aimbot angles",
        "Welcome to " .. rgb_to_hex(ui.get(refs.g.menu_color)) .. "script")
    script.start.b = ui.new_label("AA", "Anti-aimbot angles",
        "Logged as " .. rgb_to_hex(ui.get(refs.g.menu_color)) .. lua_refs.username)
    script.start.c = ui.new_label("AA", "Anti-aimbot angles", "Loading" .. rgb_to_hex(ui.get(refs.g.menu_color)) .. ".")
    script.start.d = ui.new_label("AA", "Anti-aimbot angles", "Loading" .. rgb_to_hex(ui.get(refs.g.menu_color)) .. "..")
    script.start.e = ui.new_label("AA", "Anti-aimbot angles", "Loading" .. rgb_to_hex(ui.get(refs.g.menu_color)) ..
        "...")
    script.start.f = globals.curtime()

    script.anti_aim.aa_state = ui.new_combobox("AA", "Anti-aimbot angles", "Preset", lua_refs.states_names)
    script.anti_aim.aa = {}
    for a = 1, 7 do
        script.anti_aim.aa[lua_refs.states_names[a]] = {
            yaw_base = ui.new_combobox("AA", "Anti-aimbot angles", "Yaw base" .. lua_refs.states_spaces[a], "Default",
                "Slow"),
            yaw_left = ui.new_slider("AA", "Anti-aimbot angles", "Yaw [left]" .. lua_refs.states_spaces[a], -180, 180, 0,
                true, "", 1, lua_refs.yaw_tooltip),
            yaw_right = ui.new_slider("AA", "Anti-aimbot angles", "Yaw [right]" .. lua_refs.states_spaces[a], -180, 180,
                0, true, "", 1, lua_refs.yaw_tooltip),
            yaw_center = ui.new_slider("AA", "Anti-aimbot angles", "Yaw [center]" .. lua_refs.states_spaces[a], -180, 180,
                0, true, "", 1, lua_refs.yaw_tooltip),
            jyaw = ui.new_combobox("AA", "Anti-aimbot angles", "Jitter yaw" .. lua_refs.states_spaces[a], "Center",
                "N-Way", "New-Era", "S-Mode", "Blade"),
            n_way = ui.new_slider("AA", "Anti-aimbot angles", "\nWay amount" .. lua_refs.states_spaces[a], 3, 12, 5, true,
                "", 1, lua_refs.way_tooltip),
            jyaw_add = ui.new_slider("AA", "Anti-aimbot angles", "\nJitter yaw amount" .. lua_refs.states_spaces[a], -180,
                180, 0, true, "", 1, lua_refs.yaw_tooltip),
            byaw = ui.new_combobox("AA", "Anti-aimbot angles", "Body yaw" .. lua_refs.states_spaces[a], "Default",
                "Dynamic", "Hybrid"),
            fake_left = ui.new_slider("AA", "Anti-aimbot angles", "Fake yaw limit [left]" .. lua_refs.states_spaces[a], 0,
                60, 58, true, "", 1, lua_refs.fake_tooltip),
            fake_right = ui.new_slider("AA", "Anti-aimbot angles", "Fake yaw limit [right]" .. lua_refs.states_spaces[a],
                0, 60, 58, true, "", 1, lua_refs.fake_tooltip),
            defensive = ui.new_checkbox("AA", "Anti-aimbot angles", "Force defensive" .. lua_refs.states_spaces[a]),
        }
    end
    script.anti_aim.defensive_aa = ui.new_hotkey("AA", "Anti-aimbot angles", "Defensive options")
    for a = 1, 7 do
        script.anti_aim.aa[lua_refs.states_names[a]].defensive_options = ui.new_multiselect("AA", "Anti-aimbot angles",
            "\nBreakers" .. lua_refs.states_spaces[a], "Lag flick", "Pitch breaker", "Yaw breaker")
    end
    script.anti_aim.g_aa_space = ui.new_label("AA", "Anti-aimbot angles", "\nSpace label")
    script.anti_aim.avoid_backstab = ui.new_checkbox("AA", "Anti-aimbot angles", "Anti backstab")
    script.anti_aim.legit_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Legit aa")
    script.anti_aim.left_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Left")
    script.anti_aim.right_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Right")
    script.anti_aim.back_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Back")
    script.anti_aim.forward_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Forward")
    script.anti_aim.freestand_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Freestanding")
    script.anti_aim.edge_yaw_key = ui.new_hotkey("AA", "Anti-aimbot angles", "Edge yaw")

    script.visual.crosshair_ind = ui.new_combobox("AA", "Anti-aimbot angles", "Crosshair indicators", "Disabled",
        "script", "Team skeet")
    script.visual.color_1 = ui.new_color_picker("AA", "Anti-aimbot angles", "Color_picker1", 175, 255, 0, 255)
    script.visual.color_2 = ui.new_color_picker("AA", "Anti-aimbot angles", "Color_picker2", 0, 200, 255, 255)
    script.visual.local_anims = ui.new_multiselect("AA", "Anti-aimbot angles", "Local animations", "Body lean in air",
        "Pitch zero on land", "Static legs in air", "Static move yaw", "Broken animations", "Air move", "Restate on fd")
    script.visual.lean_amount = ui.new_slider("AA", "Anti-aimbot angles", "\nBody lean amount", 0, 100, 100, true, "%")
    script.visual.color_name = ui.new_label("AA", "Anti-aimbot angles", "Color")
    script.visual.color = ui.new_color_picker("AA", "Anti-aimbot angles", "Color_picker", 255, 204, 153, 255)

    script.other.exp_tweaks = ui.new_multiselect("AA", "Anti-aimbot angles", "Exploit tweaks",
        "Charge dt in unsafe position", "Os-aa without fakelag")
    script.other.break_extrapolation = ui.new_checkbox("AA", "Anti-aimbot angles", "Break extrapolation")
    script.other.auto_tp = ui.new_hotkey("AA", "Anti-aimbot angles", "Automatic discharge dt in unsafe position")
    script.other.leg_move = ui.new_combobox("AA", "Anti-aimbot angles", "Leg movement", "Default", "Slide", "Broken")
    script.other.slowwalk_speed = ui.new_slider("AA", "Anti-aimbot angles", "Slow motion accuracy", 0, 100, 50, true,
        "%")

    local angle3d_struct = ffi.typeof("struct { float pitch; float yaw; float roll; }")
    local vec_struct = ffi.typeof("struct { float x; float y; float z; }")

    local cUserCmd =
        ffi.typeof(
            [[
        struct
        {
            uintptr_t vfptr;
            int command_number;
            int tick_count;
            $ viewangles;
            $ aimdirection;
            float forwardmove;
            float sidemove;
            float upmove;
            int buttons;
            uint8_t impulse;
            int weaponselect;
            int weaponsubtype;
            int random_seed;
            short mousedx;
            short mousedy;
            bool hasbeenpredicted;
            $ headangles;
            $ headoffset;
            bool send_packet;
        }
        ]],
            angle3d_struct,
            vec_struct,
            angle3d_struct,
            vec_struct
        )

    ffi.cdef [[
        typedef int(__thiscall* get_clipboard_text_count)(void*);
        typedef void(__thiscall* set_clipboard_text)(void*, const char*, int);
        typedef void(__thiscall* get_clipboard_text)(void*, int, const char*, int);
    ]]

    local VGUI_System010 = client.create_interface("vgui2.dll", "VGUI_System010") or
        print("Error finding VGUI_System010")
    local VGUI_System = ffi.cast(ffi.typeof('void***'), VGUI_System010)

    local get_clipboard_text_count = ffi.cast("get_clipboard_text_count", VGUI_System[0][7]) or
        print("get_clipboard_text_count Invalid")
    local set_clipboard_text = ffi.cast("set_clipboard_text", VGUI_System[0][9]) or print("set_clipboard_text Invalid")
    local get_clipboard_text = ffi.cast("get_clipboard_text", VGUI_System[0][11]) or print("get_clipboard_text Invalid")

    local client_sig = client.find_signature("client.dll", "\xB9\xCC\xCC\xCC\xCC\x8B\x40\x38\xFF\xD0\x84\xC0\x0F\x85") or
        error("client.dll!:input not found.")
    local get_cUserCmd = ffi.typeof("$* (__thiscall*)(uintptr_t ecx, int nSlot, int sequence_number)", cUserCmd)
    local input_vtbl = ffi.typeof([[struct{uintptr_t padding[8];$ GetUserCmd;}]], get_cUserCmd)
    local input = ffi.typeof([[struct{$* vfptr;}*]], input_vtbl)
    local get_input = ffi.cast(input, ffi.cast("uintptr_t**", tonumber(ffi.cast("uintptr_t", client_sig)) + 1)[0])
    local get_client_entity_bind = vtable_bind("client_panorama.dll", "VClientEntityList003", 3,
        "void*(__thiscall*)(void*,int)")
    local get_inaccuracy = vtable_thunk(483, "float(__thiscall*)(void*)")

    local functions = {}
    functions.normalize_string = (function(string)
        return string.sub(string, 10)
    end)
    functions.is_nan = (function(v)
        return tostring(v) == tostring(0 / 0)
    end)
    functions.vec_add = (function(a, b)
        return {
            a[1] + b[1],
            a[2] + b[2],
            a[3] + b[3]
        }
    end)
    functions.contains = (function(table, value)
        if table == nil then
            return false
        end
        table = ui.get(table)
        for i = 0, #table do
            if table[i] == value then
                return true
            end
        end
        return false
    end)
    functions.normalize_yaw = (function(yaw)
        while yaw > 180 do yaw = yaw - 360 end
        while yaw < -180 do yaw = yaw + 360 end
        return yaw
    end)
    functions.distance3d = (function(x1, y1, z1, x2, y2, z2)
        return math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1))
    end)
    functions.extrapolate_position = (function(xpos, ypos, zpos, ticks, player)
        local x, y, z = entity.get_prop(player, "m_vecVelocity")
        if x == nil or y == nil or z == nil then return end
        for i = 0, ticks do
            xpos = xpos + (x * globals.tickinterval())
            ypos = ypos + (y * globals.tickinterval())
            zpos = zpos + (z * globals.tickinterval())
        end
        return xpos, ypos, zpos
    end)

    functions.entity_has_c4 = (function(ent)
        local bomb = entity.get_all("CC4")[1]
        return bomb ~= nil and entity.get_prop(bomb, "m_hOwnerEntity") == ent
    end)
    functions.get_velocity = (function(player)
        local x, y, z = entity.get_prop(player, "m_vecVelocity")
        if x == nil then return end
        return math.sqrt(x * x + y * y + z * z)
    end)
    functions.is_crouching = (function(player)
        if player == nil then return end
        local flags = entity.get_prop(player, "m_fFlags")
        if flags == nil then return end
        if bit.band(flags, 4) == 4 then
            return true
        end
        return false
    end)
    functions.in_air = (function(player)
        if player == nil then return end
        local flags = entity.get_prop(player, "m_fFlags")
        if flags == nil then return end
        if bit.band(flags, 1) == 0 then
            return true
        end
        return false
    end)
    functions.get_state = (function(velocity, cmd)
        local in_jump = functions.in_air(entity.get_local_player()) or cmd.in_jump == 1
        local in_duck = functions.is_crouching(entity.get_local_player()) or cmd.in_duck == 1 or ui.get(refs.g.fake_duck)
        local in_walk = (ui.get(refs.g.slowwalk[1]) and ui.get(refs.g.slowwalk[2])) or cmd.in_speed == 1
        if velocity < 5 and not (in_jump or in_duck) then
            cnds = 1
        elseif in_jump and not in_duck then
            cnds = 4
        elseif in_jump and in_duck then
            cnds = 5
        elseif in_duck and not in_jump then
            if velocity < 5 then
                cnds = 6
            else
                cnds = 7
            end
        else
            if in_walk then
                cnds = 3
            else
                cnds = 2
            end
        end
        return cnds
    end)
    functions.get_fakelag = (function(cmd)
        if cmd.chokedcommands ~= 0 then
            last_choke_packet = cmd.chokedcommands
        else
            last_send_packet = last_choke_packet
        end
        if last_send_packet == nil then
            return 1
        else
            return last_send_packet
        end
    end)
    functions.get_entities = (function(enemy_only, alive_only)
        local enemy_only = enemy_only ~= nil and enemy_only or false
        local alive_only = alive_only ~= nil and alive_only or true
        local result = {}
        local me = entity.get_local_player()
        local player_resource = entity.get_player_resource()
        for player = 1, globals.maxplayers() do
            local is_enemy, is_alive = true, true

            if enemy_only and not entity.is_enemy(player) then is_enemy = false end
            if is_enemy then
                if alive_only and entity.get_prop(player_resource, 'm_bAlive', player) ~= 1 then is_alive = false end
                if is_alive then table.insert(result, player) end
            end
        end
        return result
    end)
    functions.max_angle = (function(player)
        local player_index = c_entity.new(player)
        local player_animstate = player_index:get_anim_state()
        local max_yaw = player_animstate.max_yaw
        local min_yaw = player_animstate.min_yaw
        --local duck_amount = player_animstate.duck_amount
        local feet_speed_forwards_or_sideways = math.max(0, math.min(1, player_animstate.feet_speed_forwards_or_sideways))
        local feet_speed_unknown_forwards_or_sideways = math.max(1,
            player_animstate.feet_speed_unknown_forwards_or_sideways)
        local value = ((player_animstate.stop_to_full_running_fraction * -0.30000001 - 0.19999999) * feet_speed_forwards_or_sideways + 1)
        --[[if duck_amount > 0 then
            value = value + duck_amount * feet_speed_unknown_forwards_or_sideways * (0.5 - value)
        end]]
        local delta_yaw = math.abs(player_animstate.max_yaw) * value
        return math.max(27, math.min(math.abs(max_yaw), delta_yaw))
    end)

    functions.get_flag_names = (function(flags_amount)
        local flags = {
            [1] = "Helmet",
            [2] = "Kevlar",
            [4] = "Helmet + Kevlar",
            [8] = "Zoom",
            [16] = "Blind",
            [32] = "Reload",
            [64] = "Bomb",
            [128] = "Vip",
            [256] = "Defuse",
            [512] = "Fakeduck",
            [1024] = "Pin pulled",
            [2048] = "Hit",
            [4096] = "Occluded",
            [8192] = "Exploiter",
            [131072] = "Defensive dt"
        }
        local indices = {}
        for i, name in pairs(flags) do
            if bit.band(flags_amount, i) == i then
                table.insert(indices, name)
            end
        end
        return indices
    end)
    functions.entity_have_flag = (function(player, flag_search)
        if player == nil then return false end
        local result = false
        local esp_data = entity.get_esp_data(player)
        local flags = functions.get_flag_names(esp_data.flags)
        for _, flag_name in pairs(flags) do
            if (flag_name ~= flag_search) then goto skip end
            result = true
            ::skip::
        end
        return result
    end)
    functions.can_desync = (function(cmd)
        if entity.get_prop(entity.get_local_player(), "m_MoveType") == 9 then
            return false
        end
        local client_weapon = entity.get_player_weapon(entity.get_local_player())
        if client_weapon == nil then
            return false
        end
        local weapon_classname = entity.get_classname(client_weapon)
        local in_use = cmd.in_use == 1
        local in_attack = cmd.in_attack == 1
        local in_attack2 = cmd.in_attack2 == 1
        if in_use then
            return false
        end
        if in_attack or in_attack2 then
            if weapon_classname:find("Grenade") then
                vars.m1_time = globals.curtime() + 0.15
            end
        end
        if vars.m1_time > globals.curtime() then
            return false
        end
        if in_attack then
            if client_weapon == nil then
                return false
            end
            if weapon_classname then
                return false
            end
            return false
        end
        return true
    end)

    functions.manual_direction = (function()
        if ui.get(script.anti_aim.back_key) or back_dir == nil then
            back_dir = true
            right_dir = false
            left_dir = false
            forward_dir = false
            manual_dir = 0
            vars.last_press = globals.realtime()
        elseif ui.get(script.anti_aim.right_key) then
            if right_dir == true and vars.last_press + 0.07 < globals.realtime() then
                back_dir = true
                right_dir = false
                left_dir = false
                forward_dir = false
                manual_dir = 0
            elseif right_dir == false and vars.last_press + 0.07 < globals.realtime() then
                right_dir = true
                back_dir = false
                left_dir = false
                forward_dir = false
                manual_dir = 90
            end
            vars.last_press = globals.realtime()
        elseif ui.get(script.anti_aim.left_key) then
            if left_dir == true and vars.last_press + 0.07 < globals.realtime() then
                back_dir = true
                right_dir = false
                left_dir = false
                forward_dir = false
                manual_dir = 0
            elseif left_dir == false and vars.last_press + 0.07 < globals.realtime() then
                left_dir = true
                back_dir = false
                right_dir = false
                forward_dir = false
                manual_dir = -90
            end
            vars.last_press = globals.realtime()
        elseif ui.get(script.anti_aim.forward_key) then
            if forward_dir == true and vars.last_press + 0.07 < globals.realtime() then
                back_dir = true
                right_dir = false
                left_dir = false
                forward_dir = false
                manual_dir = 0
            elseif forward_dir == false and vars.last_press + 0.07 < globals.realtime() then
                left_dir = false
                back_dir = false
                right_dir = false
                forward_dir = true
                manual_dir = 180
            end
            vars.last_press = globals.realtime()
        end
        return back_dir, right_dir, left_dir, forward_dir, manual_dir
    end)
    functions.modify_velocity = (function(cmd, goalspeed)
        if goalspeed <= 0 then
            return
        end
        local minimalspeed = math.sqrt((cmd.forwardmove * cmd.forwardmove) + (cmd.sidemove * cmd.sidemove))
        if minimalspeed <= 0 then
            return
        end
        if cmd.in_duck == 1 then
            goalspeed = goalspeed * 2.94117647
        end
        if minimalspeed <= goalspeed then
            return
        end
        local speedfactor = goalspeed / minimalspeed
        cmd.forwardmove = cmd.forwardmove * speedfactor
        cmd.sidemove = cmd.sidemove * speedfactor
    end)
    functions.aa_on_use = (function(cmd)
        local plocal = entity.get_local_player()
        local distance = 100
        local bomb = entity.get_all("CPlantedC4")[1]
        local bomb_x, bomb_y, bomb_z = entity.get_prop(bomb, "m_vecOrigin")
        if bomb_x ~= nil then
            local player_x, player_y, player_z = entity.get_prop(plocal, "m_vecOrigin")
            distance = functions.distance3d(bomb_x, bomb_y, bomb_z, player_x, player_y, player_z)
        end
        local team_num = entity.get_prop(plocal, "m_iTeamNum")
        local defusing = team_num == 3 and distance < 62
        local on_bombsite = entity.get_prop(plocal, "m_bInBombZone")
        local has_bomb = functions.entity_has_c4(plocal)
        local px, py, pz = client.eye_position()
        local pitch, yaw = client.camera_angles()
        local sin_pitch = math.sin(math.rad(pitch))
        local cos_pitch = math.cos(math.rad(pitch))
        local sin_yaw = math.sin(math.rad(yaw))
        local cos_yaw = math.cos(math.rad(yaw))
        local dir_vec = { cos_pitch * cos_yaw, cos_pitch * sin_yaw, -sin_pitch }
        local fraction, entindex = client.trace_line(plocal, px, py, pz, px + (dir_vec[1] * 8192),
            py + (dir_vec[2] * 8192), pz + (dir_vec[3] * 8192))
        local using = true
        if entindex ~= nil then
            if vars.classnames == nil then return end
            for i = 0, #vars.classnames do
                if entindex == nil then return end
                if entity.get_classname(entindex) == vars.classnames[i] then
                    using = false
                end
            end
        end
        if not using and not defusing then
            cmd.in_use = 0
        end
    end)
    functions.avoid_backstab = (function(target)
        if target == nil then return false end
        local players = functions.get_entities(true, true)
        local is_dangerous = false
        for _, player in pairs(players) do
            if player == nil then goto skip end
            if entity.is_dormant(player) then goto skip end
            local weapon_ent = entity.get_player_weapon(player)
            if weapon_ent == nil then goto skip end
            local weapon_idx = entity.get_prop(weapon_ent, "m_iItemDefinitionIndex")
            if weapon_idx == nil then goto skip end
            local weapon = csgo_weapons[weapon_idx]
            if weapon == nil then goto skip end
            if weapon.type == "knife" or weapon.type == "taser" then goto skip end
            if not functions.entity_have_flag(player, "Hit") then goto skip end
            is_dangerous = true
            ::skip::
        end
        local client_origin = vector(entity.get_origin(entity.get_local_player()))
        local target_origin = vector(entity.get_origin(target))
        local distance = functions.distance3d(client_origin.x, client_origin.y, client_origin.z, target_origin.x,
            target_origin.y, target_origin.z)
        if distance > 250 then return false end
        if is_dangerous then return false end
        local weapon_ent = entity.get_player_weapon(target)
        if weapon_ent == nil then return false end
        local weapon_idx = entity.get_prop(weapon_ent, "m_iItemDefinitionIndex")
        if weapon_idx == nil then return false end
        local weapon = csgo_weapons[weapon_idx]
        if weapon == nil then return false end
        return (weapon.type == "knife")
    end)
    functions.get_yaw_base = (function(target, yaw_base)
        if (target == nil) or (yaw_base == "Local view") then
            local pitch, yaw = client.camera_angles()
            return pitch, yaw
        else
            local eye_pos = vector(client.eye_position())
            local origin = vector(entity.get_origin(target))
            local aim_pos = origin + vector(0, 0, 40)
            local pitch, yaw = eye_pos:to(aim_pos):angles()
            return pitch, yaw
        end
    end)
    functions.apply_desync = (function(cmd, fake)
        local fake = math.min(fake, functions.max_angle(entity.get_local_player()))
        local target = client.current_threat()
        local usrcmd = get_input.vfptr.GetUserCmd(ffi.cast("uintptr_t", get_input), 0, cmd.command_number)
        local pitch, yaw = functions.get_yaw_base(target, ui.get(refs.aa.yaw_target))
        local can_desync = functions.can_desync(cmd)
        if can_desync then
            cmd.allow_send_packet = false
            if cmd.chokedcommands == 0 then
                cmd.yaw = functions.normalize_yaw(yaw + (180 + ui.get(refs.aa.yaw[2]))) -
                    (fake * 2) * ui.get(refs.aa.byaw[2]);
            end
        end
    end)

    functions.generate_wayjitter_tbl = (function(variant, val)
        local variants = {
            {
                {
                    {
                        [-1] = { -val, val, 0 },
                        [1] = { val, -val, 0 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, 0 },
                        [1] = { 1, -1, 0 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.50, -val * 0.50 },
                        [1] = { val, -val, -val * 0.50, val * 0.50 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1 },
                        [1] = { 1, -1, 1, -1 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.50, -val * 0.50, 0 },
                        [1] = { val, -val, -val * 0.50, val * 0.50, 0 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, 0 },
                        [1] = { 1, -1, 1, -1, 0 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.67, -val * 0.67, -val * 0.33, val * 0.33 },
                        [1] = { val, -val, -val * 0.67, val * 0.67, val * 0.33, -val * 0.33 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1 },
                        [1] = { 1, -1, 1, -1, 1, -1 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.67, -val * 0.67, -val * 0.33, val * 0.33, 0 },
                        [1] = { val, -val, -val * 0.67, val * 0.67, val * 0.33, -val * 0.33, 0 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1, 0 },
                        [1] = { 1, -1, 1, -1, 1, -1, 0 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.75, -val * 0.75, -val * 0.50, val * 0.50, val * 0.25, -val * 0.25 },
                        [1] = { val, -val, -val * 0.75, val * 0.75, val * 0.50, -val * 0.50, -val * 0.25, val * 0.25 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1, -1, 1 },
                        [1] = { 1, -1, 1, -1, 1, -1, 1, -1 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.75, -val * 0.75, -val * 0.50, val * 0.50, val * 0.25, -val * 0.25, 0 },
                        [1] = { val, -val, -val * 0.75, val * 0.75, val * 0.50, -val * 0.50, -val * 0.25, val * 0.25, 0 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1, -1, 1, 0 },
                        [1] = { 1, -1, 1, -1, 1, -1, 1, -1, 0 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.80, -val * 0.80, -val * 0.60, val * 0.60, val * 0.40, -val * 0.40, -val * 0.20, val * 0.20 },
                        [1] = { val, -val, -val * 0.80, val * 0.80, val * 0.60, -val * 0.60, -val * 0.40, val * 0.40, val * 0.20, -val * 0.20 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1, -1, 1, -1, 1 },
                        [1] = { 1, -1, 1, -1, 1, -1, 1, -1, 1, -1 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.80, -val * 0.80, -val * 0.60, val * 0.60, val * 0.40, -val * 0.40, -val * 0.20, val * 0.20, 0 },
                        [1] = { val, -val, -val * 0.80, val * 0.80, val * 0.60, -val * 0.60, -val * 0.40, val * 0.40, val * 0.20, -val * 0.20, 0 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, 0 },
                        [1] = { 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 0 },
                    },
                },
            },
            {
                {
                    {
                        [-1] = { -val, val, val * 0.83, -val * 0.83, -val * 0.67, val * 0.67, val * 0.50, -val * 0.50, -val * 0.33, val * 0.33, val * 0.17, -val * 0.17 },
                        [1] = { val, -val, -val * 0.83, val * 0.83, val * 0.67, -val * 0.67, -val * 0.50, val * 0.50, val * 0.33, -val * 0.33, -val * 0.17, val * 0.17 },
                    },
                },
                {
                    {
                        [-1] = { -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1 },
                        [1] = { 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1 },
                    },
                },
            },
        }
        return variants[variant]
    end)
    functions.get_freestand_direction = (function()
        local data = {
            side = 1,
            last_side = 0,

            last_hit = 0,
            hit_side = 0
        }
        local me = entity.get_local_player()
        if (not me or entity.get_prop(me, "m_lifeState") ~= 0) then
            return
        end
        if data.hit_side ~= 0 and globals.curtime() - data.last_hit > 5 then
            data.last_side = 0
            data.last_hit = 0
            data.hit_side = 0
        end
        local x, y, z = client.eye_position()
        local _, yaw = client.camera_angles()
        local trace_data = { left = 0, right = 0 }
        for i = yaw - 120, yaw + 120, 30 do
            if i ~= yaw then
                local rad = math.rad(i)

                local px, py, pz = x + 256 * math.cos(rad), y + 256 * math.sin(rad), z

                local fraction = client.trace_line(me, x, y, z, px, py, pz)
                local side = i < yaw and "left" or "right"

                trace_data[side] = trace_data[side] + fraction
            end
        end
        data.side = trace_data.left < trace_data.right and -1 or 1
        if data.side == data.last_side then
            return
        end
        data.last_side = data.side
        if data.hit_side ~= 0 then
            data.side = data.hit_side
        end
        local result = data.side
        return result
    end)
    functions.get_defensive_state = (function()
        local sim_time = toticks(entity.get_prop(entity.get_local_player(), "m_flSimulationTime"))
        local sim_diff = sim_time - vars.breaker.last_sim_time
        if sim_diff < 0 then
            vars.breaker.defensive_until = globals.tickcount() + math.abs(sim_diff) + toticks(client.latency())
        end
        vars.breaker.last_sim_time = sim_time
        return vars.breaker.defensive_until >= globals.tickcount()
    end)
    functions.get_simulation = (function(defensive_state)
        if vars.breaker.lag_record[globals.tickcount()] == nil then
            vars.breaker.lag_record[globals.tickcount()] = defensive_state
        end
        local send_p = true
        local simtime = toticks(entity.get_prop(entity.get_local_player(), "m_flSimulationTime"))
        if simtime >= globals.tickcount() then return true end
        for a = simtime, globals.tickcount() do
            if vars.breaker.lag_record[a] ~= nil then
                if vars.breaker.lag_record[a] == false then
                    send_p = false
                end
            end
        end
        return send_p
    end)

    functions.gradient_text = (function(r1, g1, b1, a1, r2, g2, b2, a2, text)
        local output = ''
        local len = #text - 1
        local rinc = (r2 - r1) / len
        local ginc = (g2 - g1) / len
        local binc = (b2 - b1) / len
        local ainc = (a2 - a1) / len
        for i = 1, len + 1 do
            output = output .. ('\a%02x%02x%02x%02x%s'):format(r1, g1, b1, a1, text:sub(i, i))
            r1 = r1 + rinc
            g1 = g1 + ginc
            b1 = b1 + binc
            a1 = a1 + ainc
        end
        return output
    end)
    functions.pulse_animation = (function()
        if cur_alpha == nil then
            cur_alpha = 255
            target_alpha = 0
            max_alpha = 255
            min_alpha = 0
            speed = 0.04
        end
        if (cur_alpha < min_alpha + 2) then
            target_alpha = max_alpha
        elseif (cur_alpha > max_alpha - 2) then
            target_alpha = min_alpha
        end
        cur_alpha = cur_alpha + (target_alpha - cur_alpha) * speed * (globals.absoluteframetime() * 100)
        return cur_alpha
    end)
    functions.fade_animation = (function()
        if interval == nil then
            interval = 0
            modifier = 1.65
        end
        interval = interval + (1 - modifier) * 0.7 + 0.3
        local textPulsate = math.abs(interval * 0.0175 % 2 - 1) * 255
        local fraction = (textPulsate / 255)
        local disfraction = math.abs(1 - fraction)
        return textPulsate, fraction, disfraction
    end)
    functions.faded_shadow = (function(x, y, w, h, r, g, b, a, offset, a_offset)
        renderer.rectangle(x, y, w, h, r, g, b, a)
        renderer.circle(x, y + h / 2, r, g, b, a, h / 2, 180, 0.5)
        renderer.circle(x + w, y + h / 2, r, g, b, a, h / 2, 0, 0.5)
        for i = 1, offset do
            local i_mod = i * (a_offset / offset)
            local alpha = (a / a_offset) * (a_offset - i_mod)
            renderer.rectangle(x, y - i, w, 1, r, g, b, alpha)
            renderer.rectangle(x, y - 1 + h + i, w, 1, r, g, b, alpha)
            renderer.circle_outline(x, y + h / 2, r, g, b, alpha, h / 2 + i, 90, 0.5, 1)
            renderer.circle_outline(x + w, y + h / 2, r, g, b, alpha, h / 2 + i, 270, 0.5, 1)
        end
    end)
    functions.renderer_box = (function(c, vec_min, vec_max, origin, r, g, b, a)
        if type(vec_min) ~= "table" or type(vec_max) ~= "table" or type(origin) ~= "table" then return end
        local min = functions.vec_add(vec_min, origin)
        local max = functions.vec_add(vec_max, origin)
        local points =
        {
            { min[1], min[2], min[3] },
            { min[1], max[2], min[3] },
            { max[1], max[2], min[3] },
            { max[1], min[2], min[3] },
            { min[1], min[2], max[3] },
            { min[1], max[2], max[3] },
            { max[1], max[2], max[3] },
            { max[1], min[2], max[3] },
        }
        local edges = {
            { 0, 1 }, { 1, 2 }, { 2, 3 }, { 3, 0 },
            { 5, 6 }, { 6, 7 }, { 1, 4 }, { 4, 8 },
            { 0, 4 }, { 1, 5 }, { 2, 6 }, { 3, 7 },
            { 5, 8 }, { 7, 8 }, { 3, 4 }
        }
        for i = 1, #edges do
            if points[edges[i][1]] ~= nil and points[edges[i][2]] ~= nil then
                local p1 = { client.world_to_screen(c, points[edges[i][1]][1], points[edges[i][1]][2],
                    points[edges[i][1]][3]) }
                local p2 = { client.world_to_screen(c, points[edges[i][2]][1], points[edges[i][2]][2],
                    points[edges[i][2]][3]) }

                renderer.line(p1[1], p1[2], p2[1], p2[2], r, g, b, a)
            end
        end
    end)

    functions.clipboard_import = (function()
        local clipboard_text_length = get_clipboard_text_count(VGUI_System)
        local clipboard_data = ""
        if clipboard_text_length > 0 then
            buffer = ffi.new("char[?]", clipboard_text_length)
            size = clipboard_text_length * ffi.sizeof("char[?]", clipboard_text_length)

            get_clipboard_text(VGUI_System, 0, buffer, size)

            clipboard_data = ffi.string(buffer, clipboard_text_length - 1)
        end
        return clipboard_data
    end)
    functions.clipboard_export = (function(string)
        if string then
            set_clipboard_text(VGUI_System, string, string:len())
        end
    end)
    functions.arr_to_string = (function(arr)
        arr = ui.get(arr)
        local str = ""
        for i = 1, #arr do
            str = str .. arr[i] .. (i == #arr and "" or ",")
        end
        if str == "" then
            str = "-"
        end
        return str
    end)
    functions.str_to_sub = (function(input, sep)
        local t = {}
        for str in string.gmatch(input, "([^" .. sep .. "]+)") do
            t[#t + 1] = string.gsub(str, "\n", "")
        end
        return t
    end)
    functions.to_boolean = (function(str)
        if str == "true" then
            return true
        else
            return false
        end
    end)
    functions.import_cfg = (function(input)
        local tbl = functions.str_to_sub(input, "|")
        for i = 1, 7 do
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].yaw_base, tbl[1 + (12 * (i - 1))])
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].yaw_left, tonumber(tbl[2 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].yaw_right, tonumber(tbl[3 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].yaw_center, tonumber(tbl[4 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].jyaw, tbl[5 + (12 * (i - 1))])
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].n_way, tonumber(tbl[6 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].jyaw_add, tonumber(tbl[7 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].byaw, tbl[8 + (12 * (i - 1))])
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].fake_left, tonumber(tbl[9 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].fake_right, tonumber(tbl[10 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].defensive, functions.to_boolean(tbl[11 + (12 * (i - 1))]))
            ui.set(script.anti_aim.aa[lua_refs.states_names[i]].defensive_options,
                functions.str_to_sub(tbl[12 + (12 * (i - 1))], ","))
        end
    end)
    functions.export_cfg = (function()
        local str = ""
        for i = 1, 7 do
            str = str .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].yaw_base)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].yaw_left)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].yaw_right)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].yaw_center)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].jyaw)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].n_way)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].jyaw_add)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].byaw)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].fake_left)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].fake_right)) .. "|"
                .. tostring(ui.get(script.anti_aim.aa[lua_refs.states_names[i]].defensive)) .. "|"
                .. functions.arr_to_string(script.anti_aim.aa[lua_refs.states_names[i]].defensive_options) .. "|"
        end
        return str
    end)

    if database.read("[script] preset list") == nil then
        database.write("[script] preset list", { "script" })
    end
    http.get("https://pastebin.com/raw/4Eb7nmMY", function(success, response)
        if not success or response.status ~= 200 then
            return
        end

        database.write("[script] script preset", response.body)
    end)
    local preset_table = database.read("[script] preset list")
    script.config.preset_list_menu = ui.new_listbox("AA", "Anti-aimbot angles", "Preset list", preset_table)
    ui.set(script.config.preset_list_menu, 0)
    script.config.load_preset_but = ui.new_button("AA", "Anti-aimbot angles", "Load preset", function()
        local r, g, b, a = ui.get(script.visual.color)
        local preset_amount = preset_table[ui.get(script.config.preset_list_menu) + 1]
        if preset_amount == nil then
            local preset_txt = database.read("[script] script preset")
            functions.import_cfg(preset_txt)
            client.color_log(r, g, b, "script preset from cloud was imported")
            return
        end
        local preset_txt = database.read("[script] " .. preset_amount .. " preset")
        functions.import_cfg(preset_txt)
        if preset_amount == "script" then
            client.color_log(r, g, b, "script preset from cloud was imported")
        else
            client.color_log(r, g, b, "Your custom preset from database was imported")
        end
    end)
    script.config.delete_preset_but = ui.new_button("AA", "Anti-aimbot angles", "Delete preset", function()
        local r, g, b, a = ui.get(script.visual.color)
        if ui.get(script.config.preset_list_menu) == nil then
            error("Got an error while trying to delete a preset")
            return
        end
        local preset_amount = (ui.get(script.config.preset_list_menu) + 1)
        local pname = preset_table[preset_amount]
        if pname == nil or pname == "script" then
            error("Got an error while trying to delete a preset")
            return
        end
        local tbl_rem = preset_table
        table.remove(tbl_rem, preset_amount)
        database.write("[script] preset list", tbl_rem)
        ui.update(script.config.preset_list_menu, database.read("[script] preset list"))
        client.color_log(r, g, b, "Your custom preset from database was deleted")
    end)
    script.config.export_database_cfg_to_clip_but = ui.new_button("AA", "Anti-aimbot angles",
        "Export preset to clipboard", function()
            local r, g, b, a = ui.get(script.visual.color)
            local preset_amount = preset_table[ui.get(script.config.preset_list_menu) + 1]
            if preset_amount == nil or preset_amount == "script" then
                error("Got an error while trying to load a preset")
                return
            end
            local preset_txt = database.read("[script] " .. preset_amount .. " preset")
            functions.clipboard_export(preset_txt)
            client.color_log(r, g, b, "Your custom preset from database was exported to clipboard")
        end)
    script.config.presetname = ui.new_textbox("AA", "Anti-aimbot angles", "Please enter preset name")
    script.config.save_preset_but = ui.new_button("AA", "Anti-aimbot angles", "Save preset", function()
        local r, g, b, a = ui.get(script.visual.color)
        local pname = ui.get(script.config.presetname)
        local name_is_space = true
        for a = 1, #lua_refs.symbols do
            local symbol = lua_refs.symbols[a]
            if string.find(ui.get(script.config.presetname), tostring(symbol)) == nil then goto skip end
            name_is_space = false
            ::skip::
        end
        if name_is_space then
            local preset_amount = (ui.get(script.config.preset_list_menu) + 1)
            local pname = preset_table[preset_amount]
            if pname == nil or pname == "script" then
                error("Got an error while trying to save a preset")
                return
            end
            local export = functions.export_cfg()
            database.write("[script] " .. pname .. " preset", export)
            client.color_log(r, g, b, "Your custom preset was exported to database")
            return
        end
        if ui.get(script.config.presetname) == "script" then
            error("Got an error while trying to save a preset")
            return
        end
        for p = 1, #preset_table do
            local pname2 = preset_table[p]
            if pname2 == pname then
                table.remove(preset_table, p)
            end
        end
        local tbl_rem = preset_table
        tbl_rem[#tbl_rem + 1] = pname
        local export = functions.export_cfg()
        database.write("[script] " .. pname .. " preset", export)
        database.write("[script] preset list", tbl_rem)
        ui.update(script.config.preset_list_menu, database.read("[script] preset list"))
        client.color_log(r, g, b, "Your custom preset was exported to database")
    end)
    script.config.import_cfg_from_clip_but = ui.new_button("AA", "Anti-aimbot angles", "Import preset from clipboard",
        function()
            local r, g, b, a = ui.get(script.visual.color)
            functions.import_cfg(functions.clipboard_import())
            client.color_log(r, g, b, "Your preset from clipboard was imported")
        end)
    script.config.export_cfg_to_clip_but = ui.new_button("AA", "Anti-aimbot angles", "Export preset to clipboard",
        function()
            local r, g, b, a = ui.get(script.visual.color)
            functions.clipboard_export(functions.export_cfg())
            client.color_log(r, g, b, "Your preset was exported to clipboard")
        end)
    script.config.cmd_label_a = ui.new_label("AA", "Anti-aimbot angles", "[commands]")
    script.config.cmd_label_b = ui.new_label("AA", "Anti-aimbot angles", "     >> //import")
    script.config.cmd_label_c = ui.new_label("AA", "Anti-aimbot angles", "     >> //load")
    script.config.cmd_label_d = ui.new_label("AA", "Anti-aimbot angles", "     >> //export")
    script.config.cmd_label_e = ui.new_label("AA", "Anti-aimbot angles", "     >> //save")

    client.set_event_callback("console_input", function(input)
        if input == "//export" or input == "//save" then
            local r, g, b, a = ui.get(script.visual.color)
            functions.clipboard_export(functions.export_cfg())
            client.color_log(r, g, b, "Your preset was exported to clipboard")
        end
        if input == "//import" or input == "//load" then
            local r, g, b, a = ui.get(script.visual.color)
            functions.import_cfg(functions.clipboard_import())
            client.color_log(r, g, b, "Your preset from clipboard was imported")
        end
    end)

    local lua_functional = {
        create_move = (function(cmd)
            local target = client.current_threat()
            local self_index = c_entity.new(entity.get_local_player())
            local usrcmd = get_input.vfptr.GetUserCmd(ffi.cast("uintptr_t", get_input), 0, cmd.command_number)
            local anim_state = self_index:get_anim_state()
            local freestand_dir = functions.get_freestand_direction()
            local back_dir, right_dir, left_dir, forward_dir, manual_dir = functions.manual_direction()
            local velocity = functions.get_velocity(entity.get_local_player())
            local state = lua_refs.states_names[functions.get_state(velocity, cmd)]
            local fakelag_amount = functions.get_fakelag(cmd)

            local doubletap_ref = (ui.get(refs.g.dt[1]) and ui.get(refs.g.dt[2])) and not ui.get(refs.g.fake_duck)
            local osaa_ref = (ui.get(refs.g.os_aa[1]) and ui.get(refs.g.os_aa[2])) and
                not (ui.get(refs.g.fake_duck) or doubletap_ref)
            local is_fakelag = (fakelag_amount ~= 1) or (cmd.chokedcommands > 1)

            local jitter_amount = ui.get(script.anti_aim.aa[state].jyaw_add)
            local breaker_amount = (functions.contains(script.anti_aim.aa[state].defensive_options, "Pitch breaker") or functions.contains(script.anti_aim.aa[state].defensive_options, "Yaw breaker")) and
                ui.get(script.anti_aim.aa[state].defensive) and ui.get(script.anti_aim.defensive_aa)

            local en_ticks_modifier = (ui.get(script.anti_aim.aa[state].yaw_base) == "Slow") and 2 or 1
            local en_antiaim_ticks = {
                ["Center"] = 2,
                ["N-Way"] = ui.get(script.anti_aim.aa[state].n_way),
                ["New-Era"] = 12,
                ["S-Mode"] = 12,
                ["Blade"] = 6
            }
            local en_randomize = { ["Center"] = 1, ["N-Way"] = 1, ["New-Era"] = 4, ["S-Mode"] = 4, ["Blade"] = 4 }
            local en_freestand = { ["Default"] = (vars.aa.default_side), ["Dynamic"] = freestand_dir, ["Hybrid"] = (functions.entity_have_flag(target, "Hit") and -freestand_dir or freestand_dir) }
            local en_jitter = {
                ["Center"] = {
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.50 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.50 },
                    }
                },
                ["N-Way"] = functions.generate_wayjitter_tbl(ui.get(script.anti_aim.aa[state].n_way) - 2,
                    jitter_amount * 0.50)[1],
                ["New-Era"] = {
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.25, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.25, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0 },
                    },
                },
                ["S-Mode"] = {
                    {
                        [-1] = { -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25 },
                        [1] = { jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25, jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25 },
                        [1] = { jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25, -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25, -jitter_amount * 0.25, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0, jitter_amount * 0.50 },
                        [1] = { jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25, jitter_amount * 0.25, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0, -jitter_amount * 0.50 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.25, 0, jitter_amount * 0.25, jitter_amount * 0.25, 0, -jitter_amount * 0.25, jitter_amount * 0.50, 0, -jitter_amount * 0.50 },
                        [1] = { jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.25, 0, -jitter_amount * 0.25, -jitter_amount * 0.25, 0, jitter_amount * 0.25, -jitter_amount * 0.50, 0, jitter_amount * 0.50 },
                    },
                },
                ["Blade"] = {
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.50, 0 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.50, 0 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.50, 0, jitter_amount * 0.50, -jitter_amount * 0.50, 0 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.50, 0, -jitter_amount * 0.50, jitter_amount * 0.50, 0 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.50, 0, -jitter_amount * 0.50, 0, jitter_amount * 0.50 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.50, 0, jitter_amount * 0.50, 0, -jitter_amount * 0.50 },
                    },
                    {
                        [-1] = { -jitter_amount * 0.50, jitter_amount * 0.50, 0, jitter_amount * 0.50, 0, -jitter_amount * 0.50 },
                        [1] = { jitter_amount * 0.50, -jitter_amount * 0.50, 0, -jitter_amount * 0.50, 0, jitter_amount * 0.50 },
                    },
                },
            }
            local en_body = {
                ["Center"] = {
                    {
                        [-1] = { -1, 1 },
                        [1] = { 1, -1 },
                    }
                },
                ["N-Way"] = functions.generate_wayjitter_tbl(ui.get(script.anti_aim.aa[state].n_way) - 2,
                    jitter_amount * 0.50)[2],
                ["New-Era"] = {
                    {
                        [-1] = { -1, -1, 0, 1, 1, 0, -1, -1, 0, 1, 1, 0 },
                        [1] = { 1, 1, 0, -1, -1, 0, 1, 1, 0, -1, -1, 0 },
                    },
                    {
                        [-1] = { -1, -1, 0, 1, 1, 0, 1, 1, 0, -1, -1, 0 },
                        [1] = { 1, 1, 0, -1, -1, 0, -1, -1, 0, 1, 1, 0 },
                    },
                    {
                        [-1] = { -1, -1, 0, 1, 1, 0, 1, 1, 0, -1, -1, 0 },
                        [1] = { 1, 1, 0, -1, -1, 0, -1, -1, 0, 1, 1, 0 },
                    },
                    {
                        [-1] = { -1, -1, 0, 1, 1, 0, -1, -1, 0, 1, 1, 0 },
                        [1] = { 1, 1, 0, -1, -1, 0, 1, 1, 0, -1, -1, 0 },
                    },
                },
                ["S-Mode"] = {
                    {
                        [-1] = { -1, 0, 1, 1, 0, -1, -1, 0, 1, 1, 0, -1 },
                        [1] = { 1, 0, -1, -1, 0, 1, 1, 0, -1, -1, 0, 1 },
                    },
                    {
                        [-1] = { -1, 0, 1, 1, 0, -1, 1, 0, -1, -1, 0, 1 },
                        [1] = { 1, 0, -1, -1, 0, 1, -1, 0, 1, 1, 0, -1 },
                    },
                    {
                        [-1] = { -1, 0, 1, 1, 0, -1, 1, 0, -1, -1, 0, 1 },
                        [1] = { 1, 0, -1, -1, 0, 1, -1, 0, 1, 1, 0, -1 },
                    },
                    {
                        [-1] = { -1, 0, 1, 1, 0, -1, -1, 0, 1, 1, 0, -1 },
                        [1] = { 1, 0, -1, -1, 0, 1, 1, 0, -1, -1, 0, 1 },
                    },
                },
                ["Blade"] = {
                    {
                        [-1] = { -1, 1, 0, -1, 1, 0 },
                        [1] = { 1, -1, 0, 1, -1, 0 },
                    },
                    {
                        [-1] = { -1, 1, 0, 1, -1, 0 },
                        [1] = { 1, -1, 0, -1, 1, 0 },
                    },
                    {
                        [-1] = { -1, 1, 0, -1, 0, 1 },
                        [1] = { 1, -1, 0, 1, 0, -1 },
                    },
                    {
                        [-1] = { -1, 1, 0, 1, 0, -1 },
                        [1] = { 1, -1, 0, -1, 0, 1 },
                    },
                },
            }
            local en_yaw = {
                [-1] = ui.get(script.anti_aim.aa[state].yaw_left),
                [0] = ui.get(script.anti_aim.aa[state]
                    .yaw_center),
                [1] = ui.get(script.anti_aim.aa[state].yaw_right)
            }
            local en_fake = {
                [-1] = ui.get(script.anti_aim.aa[state].fake_left),
                [0] = 0,
                [1] = ui.get(script
                    .anti_aim.aa[state].fake_right)
            }
            local en_safe_head = { { [1] = 10, [0] = 3, [-1] = 0 }, { [1] = 10, [0] = 1, [-1] = -3 }, { [1] = 10, [0] = 1, [-1] = 0 }, { [1] = 10, [0] = 4, [-1] = -3 }, { [1] = 15, [0] = 7, [-1] = 0 }, { [1] = 14, [0] = 7, [-1] = -10 }, { [1] = 14, [0] = 7, [-1] = -10 } }
            local en_leg_move = { ["Default"] = "Never slide", ["Slide"] = "Always slide", ["Broken"] = (vars.jitter_bool and "Always slide" or "Never slide") }
            local en_slow_ticks = { 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12 }

            if cmd.chokedcommands == 0 then
                if ((vars.aa.antiaim_ticks + 1) > en_antiaim_ticks[ui.get(script.anti_aim.aa[state].jyaw)] * en_ticks_modifier) then
                    vars.aa.antiaim_ticks = 1
                    vars.aa.randomize = math.random(1, en_randomize[ui.get(script.anti_aim.aa[state].jyaw)])
                    if vars.aa.randomize == vars.aa.pervious_randomize then
                        vars.aa.randomize = math.random(1, en_randomize[ui.get(script.anti_aim.aa[state].jyaw)])
                    end
                else
                    vars.aa.pervious_randomize = vars.aa.randomize
                    vars.aa.antiaim_ticks = (vars.aa.antiaim_ticks + 1)
                end
            end
            if ui.get(script.anti_aim.legit_key) then functions.aa_on_use(cmd) end
            if state == "Walk" then functions.modify_velocity(cmd, 76 - 75 * (ui.get(script.other.slowwalk_speed) / 100)) end

            local is_defensive = (vars.breaker.defensive > 1) and
                (vars.breaker.defensive < (12 + toticks(client.latency() / 4)))
            local defensive_state = functions.get_defensive_state()
            local is_lag = (is_defensive and defensive_state) and
                ((globals.tickcount() - toticks(entity.get_prop(entity.get_local_player(), "m_flSimulationTime"))) >= 0)
            local is_avoid_backstab = ui.get(script.anti_aim.avoid_backstab) and functions.avoid_backstab(target)
            local lag_simulation = functions.get_simulation(is_lag)
            local antiaim_ticks = (en_ticks_modifier == 1) and
                math.min(en_antiaim_ticks[ui.get(script.anti_aim.aa[state].jyaw)] * en_ticks_modifier, vars.aa
                    .antiaim_ticks) or
                en_slow_ticks
                [math.min(en_antiaim_ticks[ui.get(script.anti_aim.aa[state].jyaw)] * en_ticks_modifier, vars.aa.antiaim_ticks)]
            local randomize = math.min(en_randomize[ui.get(script.anti_aim.aa[state].jyaw)], vars.aa.randomize)
            local freestand = en_freestand[ui.get(script.anti_aim.aa[state].byaw)]

            local n_jitter = en_jitter[ui.get(script.anti_aim.aa[state].jyaw)][randomize][freestand][antiaim_ticks]
            local n_body = en_body[ui.get(script.anti_aim.aa[state].jyaw)][randomize][freestand][antiaim_ticks]
            local n_safe_head = en_safe_head[functions.get_state(velocity, cmd)][freestand_dir]
            local n_yaw = en_yaw[n_body]
            local n_fake = en_fake[n_body]

            if cmd.chokedcommands == 0 then
                vars.aa.yaw_amount = (n_yaw + n_jitter)
                vars.aa.safe_head_amount = n_safe_head
                vars.aa.body_amount = n_body
            end

            local body_amount = functions.normalize_yaw(vars.aa.body_amount)
            local yaw_amount = functions.normalize_yaw(manual_dir + vars.aa.yaw_amount)
            local safe_head_amount = functions.normalize_yaw(manual_dir + vars.aa.safe_head_amount)

            ui.set(refs.aa.yaw[1], "180")
            ui.set(refs.aa.roll_aa, 0)
            ui.set(refs.aa.fake_peek[1], false)
            ui.set(refs.aa.jyaw[1], "Off")
            ui.set(refs.aa.jyaw[2], 0)
            ui.set(refs.aa.yaw_target,
                ((back_dir or is_avoid_backstab) and not (ui.get(script.anti_aim.legit_key) or ui.is_menu_open())) and
                "At targets" or "Local view")
            ui.set(refs.aa.fs[2], "Always On")
            ui.set(refs.g.fl_limit,
                (functions.contains(script.other.exp_tweaks, "Os-aa without fakelag") and osaa_ref) and 1 or 14)
            ui.set(refs.g.dt[3], "Defensive")
            ui.set(refs.aa.leg_move, en_leg_move[ui.get(script.other.leg_move)])
            if functions.contains(script.other.exp_tweaks, "Charge dt in unsafe position") then
                local doubletap_ref = ui.get(refs.g.dt[1]) and ui.get(refs.g.dt[2])
                local tickbase = globals.tickcount() - entity.get_prop(entity.get_local_player(), "m_nTickBase")
                if not doubletap_ref then vars.was_charged = false end
                if (vars.was_charged == nil) and (tickbase > 0) and doubletap_ref then vars.was_charged = true end
                if (tickbase < 0) and doubletap_ref and not vars.was_charged then
                    ui.set(refs.g.rage_box[2], "On hotkey")
                    vars.was_charged = nil
                else
                    ui.set(refs.g.rage_box[2], "Always on")
                end
            end

            if (ui.get(script.other.auto_tp) and functions.entity_have_flag(target, "Hit")) and target ~= nil then
                local weapon = entity.get_player_weapon(target)
                local hitchance = 100 - get_inaccuracy(get_client_entity_bind(weapon)) * 100
                local nextattack_time = math.max(entity.get_prop(weapon, "m_flNextPrimaryAttack"),
                    entity.get_prop(weapon, "m_flNextSecondaryAttack"))
                local can_fire = nextattack_time <= globals.curtime()
                if hitchance > 85 and can_fire then
                    local def_lag = (vars.breaker.defensive > 1) and (vars.breaker.defensive < 14)
                    local delay_time = (def_lag and 0.019 or 0.009)
                    if (globals.curtime() - vars.dt_time) > delay_time then
                        ui.set(refs.g.dt[1], false)
                    end
                else
                    vars.dt_time = globals.curtime()
                end
            else
                vars.dt_time = globals.curtime()
                ui.set(refs.g.dt[1], true)
            end

            if ui.get(script.anti_aim.aa[state].defensive) and doubletap_ref and not is_fakelag then
                cmd.force_defensive = true
                cmd.allow_send_packet = (breaker_amount and back_dir) and lag_simulation or false
            end

            if ui.get(script.anti_aim.legit_key) then
                ui.set(refs.aa.pitch, "Off")
                ui.set(refs.aa.yaw[2], 180)
                ui.set(refs.aa.byaw[1], is_fakelag and "Static" or "Jitter")
                ui.set(refs.aa.byaw[2], is_fakelag and -freestand_dir or 0)
                ui.set(refs.aa.fs[1], false)
                ui.set(refs.aa.edge_yaw, false)
                vars.aa.pervious_yaw = 180
            elseif is_avoid_backstab and not ui.is_menu_open() then
                ui.set(refs.aa.pitch, "Minimal")
                ui.set(refs.aa.yaw[2], functions.normalize_yaw(180 + (is_fakelag and safe_head_amount or yaw_amount)))
                ui.set(refs.aa.byaw[1], "Static")
                ui.set(refs.aa.byaw[2], body_amount)
                ui.set(refs.aa.fs[1], false)
                ui.set(refs.aa.edge_yaw, false)
                vars.aa.pervious_yaw = 180
            elseif doubletap_ref and functions.contains(script.anti_aim.aa[state].defensive_options, "Lag flick") and ui.get(script.anti_aim.aa[state].defensive) and ui.get(script.anti_aim.defensive_aa) and (vars.breaker.defensive > 9) and defensive_state and not is_fakelag then
                ui.set(refs.aa.pitch, "Minimal")
                ui.set(refs.aa.yaw[2], 90 * freestand_dir)
                ui.set(refs.aa.byaw[2], 180)
                ui.set(refs.aa.fs[1], false)
                ui.set(refs.aa.edge_yaw, false)
            elseif breaker_amount and back_dir and doubletap_ref and is_lag and not is_fakelag then
                local abs_lag_yaw = math.abs(functions.normalize_yaw(110 + vars.breaker.yaw) * vars.breaker.side)
                local abs_main_yaw = math.abs(vars.aa.pervious_yaw)
                local max_main_yaw_90 = abs_main_yaw > 90 and math.abs(180 - abs_main_yaw) or abs_main_yaw
                local max_lag_yaw_90 = abs_lag_yaw > 90 and math.abs(180 - abs_lag_yaw) or abs_lag_yaw
                ui.set(refs.aa.yaw[2],
                    functions.contains(script.anti_aim.aa[state].defensive_options, "Yaw breaker") and
                    (((max_main_yaw_90 - 20) < max_lag_yaw_90 and (max_main_yaw_90 + 20) > max_lag_yaw_90) and ((yaw_amount < 0) and math.abs(functions.normalize_yaw(110 + vars.breaker.yaw)) or -math.abs(functions.normalize_yaw(110 + vars.breaker.yaw))) or functions.normalize_yaw(110 + vars.breaker.yaw) * vars.breaker.side) or
                    yaw_amount)
                ui.set(refs.aa.pitch,
                    functions.contains(script.anti_aim.aa[state].defensive_options, "Pitch breaker") and "Up" or
                    "Minimal")
                ui.set(refs.aa.byaw[1], "Jitter")
                ui.set(refs.aa.byaw[2], 0)
                ui.set(refs.aa.fs[1], false)
                ui.set(refs.aa.edge_yaw, false)
            elseif functions.entity_have_flag(target, "Hit") and ((not back_dir) or ui.get(script.anti_aim.freestand_key)) then
                ui.set(refs.aa.pitch, "Minimal")
                ui.set(refs.aa.yaw[2], manual_dir)
                ui.set(refs.aa.byaw[1], "Static")
                ui.set(refs.aa.byaw[2], 180)
                ui.set(refs.aa.fs[1], (back_dir and ui.get(script.anti_aim.freestand_key)))
                ui.set(refs.aa.edge_yaw, (back_dir and ui.get(script.anti_aim.edge_yaw_key)))
                vars.aa.pervious_yaw = manual_dir
            else
                ui.set(refs.aa.pitch, "Minimal")
                ui.set(refs.aa.yaw[2], is_fakelag and safe_head_amount or yaw_amount)
                ui.set(refs.aa.byaw[1], (cmd.chokedcommands ~= 0 and not is_fakelag) and "Off" or "Static")
                ui.set(refs.aa.byaw[2], is_fakelag and freestand_dir or body_amount)
                ui.set(refs.aa.fs[1], (back_dir and ui.get(script.anti_aim.freestand_key)))
                ui.set(refs.aa.edge_yaw, (back_dir and ui.get(script.anti_aim.edge_yaw_key)))
                if not is_fakelag then
                    functions.apply_desync(cmd, n_fake)
                end
                vars.aa.pervious_yaw = is_fakelag and safe_head_amount or yaw_amount
            end

            if ui.get(script.other.break_extrapolation) then
                local sim_time = toticks(entity.get_prop(entity.get_local_player(), "m_flSimulationTime"))
                local last_tick = globals.tickcount() - sim_time
                if last_tick < 0 then
                    if usrcmd.hasbeenpredicted then
                        ui.set(refs.g.dt[3], "Offensive")
                    end
                    usrcmd.hasbeenpredicted = false
                end
            end
        end),
        render = (function(ctx)
            local scrsize_x, scrsize_y = client.screen_size()
            local center_x, center_y = scrsize_x / 2, scrsize_y / 2
            local r, g, b, a = ui.get(script.visual.color)
            local r1, g1, b1, a1 = ui.get(script.visual.color_1)
            local r2, g2, b2, a2 = ui.get(script.visual.color_2)
            local target = client.current_threat()
            local velocity = functions.get_velocity(entity.get_local_player())
            local state = lua_refs.states_names[functions.get_state(velocity, cmd_debug)]
            local back_dir, right_dir, left_dir, forward_dir, manual_dir = functions.manual_direction()
            local freestand_dir = functions.get_freestand_direction()

            for _, t in pairs(tween_table) do
                t:update(globals.frametime())
            end

            local fade, fraction, disfraction = functions.fade_animation()
            local pulse_alpha = functions.pulse_animation()

            local water_shadow = math.max((fade / 255), (math.abs(fade - 255) / 255))
            local water_text = functions.gradient_text(r * fraction, g * fraction, b * fraction, fade, r * disfraction,
                g * disfraction, b * disfraction, math.abs(fade - 255), "script")
            local water_measure = renderer.measure_text("b", "script")

            if entity.is_alive(entity.get_local_player()) then
                local en_freestand = { ["Default"] = 1, ["Dynamic"] = freestand_dir, ["Hybrid"] = (functions.entity_have_flag(target, "Hit") and -freestand_dir or freestand_dir) }
                local freestand = en_freestand[ui.get(script.anti_aim.aa[state].byaw)]
                if ui.get(script.visual.crosshair_ind) == "script" then
                    local water_text = functions.gradient_text(r1 * fraction, g1 * fraction, b1 * fraction, fade,
                        r1 * disfraction, g1 * disfraction, b1 * disfraction, math.abs(fade - 255), "script")

                    local left_arrow_measure = renderer.measure_text("+", "<")
                    local right_arrow_measure = renderer.measure_text("+", ">")

                    if ui.get(script.anti_aim.aa[state].byaw) ~= "Default" then
                        renderer.text(center_x - 50 - left_arrow_measure, center_y - 16, (freestand == 1) and r1 or 170,
                            (freestand == 1) and g1 or 170, (freestand == 1) and b1 or 170,
                            (freestand == 1) and 255 or 100, "+", 0, "<")
                        renderer.text(center_x + 50, center_y - 16, (freestand == -1) and r1 or 170,
                            (freestand == -1) and g1 or 170, (freestand == -1) and b1 or 170,
                            (freestand == -1) and 255 or 100, "+", 0, ">")
                        if freestand ~= freestand_dir then
                            if freestand == 1 then
                                renderer.text(center_x - 50 - left_arrow_measure * 2, center_y - 16, r1, g1, b1, 255, "+",
                                    0, "<")
                            else
                                renderer.text(center_x + 50 + right_arrow_measure, center_y - 16, r1, g1, b1, 255, "+", 0,
                                    ">")
                            end
                        end
                    end

                    local fake_amount = math.abs(vars.fake_amount)
                    tween_table.fake_amount = tween.new(1, tween_data, { fake_amount = fake_amount }, "linear")
                    local fake_amount = tween_data.fake_amount

                    local desync_str_measure = renderer.measure_text("b", tostring(math.floor(fake_amount * 10) / 10))

                    renderer.text(center_x - desync_str_measure / 2, center_y + 25, r2, g2, b2, 255, "b", 0,
                        tostring(math.floor(fake_amount * 10) / 10) .. "°")
                    renderer.gradient(center_x, center_y + 43, -fake_amount, 2, r1, g1, b1, 255, r1, g1, b1, 0, true)
                    renderer.gradient(center_x, center_y + 43, fake_amount, 2, r1, g1, b1, 255, r1, g1, b1, 0, true)
                    functions.faded_shadow(center_x + 3 - water_measure / 2, center_y + 51, water_measure - 6, 8, r1, g1,
                        b1, 20 * water_shadow, 6, 6)
                    renderer.text(center_x - water_measure / 2, center_y + 48, 35, 35, 35, 255, "b", 0, "script")
                    renderer.text(center_x - water_measure / 2, center_y + 48, 255, 255, 255, 255, "b", 0, water_text)
                elseif ui.get(script.visual.crosshair_ind) == "Team skeet" then
                    renderer.triangle(center_x + 55, center_y + 2, center_x + 42, center_y - 7, center_x + 42,
                        center_y + 11,
                        right_dir and r1 or 35,
                        right_dir and g1 or 35,
                        right_dir and b1 or 35,
                        right_dir and 255 or 150)

                    renderer.triangle(center_x - 55, center_y + 2, center_x - 42, center_y - 7, center_x - 42,
                        center_y + 11,
                        left_dir and r1 or 35,
                        left_dir and g1 or 35,
                        left_dir and b1 or 35,
                        left_dir and 255 or 150)

                    renderer.rectangle(center_x + 38, center_y - 7, 2, 18,
                        ((freestand == -1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and r2 or 35,
                        ((freestand == -1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and g2 or 35,
                        ((freestand == -1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and b2 or 35,
                        ((freestand == -1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and 255 or 150)
                    renderer.rectangle(center_x - 40, center_y - 7, 2, 18,
                        ((freestand == 1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and r2 or 35,
                        ((freestand == 1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and g2 or 35,
                        ((freestand == 1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and b2 or 35,
                        ((freestand == 1) and ui.get(script.anti_aim.aa[state].byaw) ~= "Default") and 255 or 150)
                end
            end

            functions.faded_shadow(center_x + 3 - water_measure / 2, scrsize_y - 17, water_measure - 6, 8, r, g, b,
                20 * water_shadow, 6, 6)
            renderer.text(center_x - water_measure / 2, scrsize_y - 20, 35, 35, 35, 255, "b", 0, "script")
            renderer.text(center_x - water_measure / 2, scrsize_y - 20, 255, 255, 255, 255, "b", 0, water_text)
        end),
        animation = (function()
            if entity.get_local_player() == nil then return end
            if functions.in_air(entity.get_local_player()) then
                vars.air_ticks = globals.tickcount()
            end

            local self_index = c_entity.new(entity.get_local_player())

            local animlayer_6 = self_index:get_anim_overlay(6)
            local animlayer_12 = self_index:get_anim_overlay(12)

            local vel_x, vel_y, vel_z = entity.get_prop(entity.get_local_player(), "m_vecVelocity")

            if functions.contains(script.visual.local_anims, "Body lean in air") then
                if ((math.abs(vel_x) > 1.1) or (math.abs(vel_y) > 1.1)) and (vars.sidemove ~= 0) and functions.in_air(entity.get_local_player()) then
                    animlayer_12.weight = ui.get(script.visual.lean_amount) / 100
                end
            end

            if functions.contains(script.visual.local_anims, "Pitch zero on land") then
                local on_ground = bit.band(entity.get_prop(entity.get_local_player(), "m_fFlags"), 1)

                if on_ground and ((vars.air_ticks + 5) < globals.tickcount()) and ((vars.air_ticks + toticks(1)) > globals.tickcount()) and not functions.in_air(entity.get_local_player()) then
                    entity.set_prop(entity.get_local_player(), "m_flPoseParameter", 0.5, 12)
                end
            end

            if functions.contains(script.visual.local_anims, "Static legs in air") then
                entity.set_prop(entity.get_local_player(), "m_flPoseParameter", 1, 6)
            end

            if functions.contains(script.visual.local_anims, "Static move yaw") then
                entity.set_prop(entity.get_local_player(), "m_flPoseParameter", 1, 7)
            end

            if functions.contains(script.visual.local_anims, "Broken animations") then
                entity.set_prop(entity.get_local_player(), "m_flPoseParameter", math.random(0, 10) / 10, 3)
                entity.set_prop(entity.get_local_player(), "m_flPoseParameter", math.random(0, 10) / 10, 7)
                entity.set_prop(entity.get_local_player(), "m_flPoseParameter", math.random(0, 10) / 10, 6)
            end

            if functions.contains(script.visual.local_anims, "Air move") then
                if functions.in_air(entity.get_local_player()) then
                    animlayer_6.weight = 1
                end
            end

            if functions.contains(script.visual.local_anims, "Restate on fd") then
                if ui.get(refs.g.fake_duck) then
                    entity.set_prop(entity.get_local_player(), "m_flPoseParameter",
                        (globals.tickcount() % 6) > 2 and 1 or 0, 16)
                    entity.set_prop(entity.get_local_player(), "m_flPoseParameter",
                        (globals.tickcount() % 6) > 2 and 1 or 0, 17)
                end
            end
        end)
    }

    client.set_event_callback("run_command", function(cmd)
        local player_index = c_entity.new(entity.get_local_player())
        local anim_state = player_index:get_anim_state()
        local pitch, yaw = client.camera_angles()

        vars.breaker.cmd = cmd.command_number
        if cmd.chokedcommands == 0 then
            vars.jitter_bool = not vars.jitter_bool
            vars.jitter_value = vars.jitter_bool and 1 or -1
            vars.view_yaw = functions.normalize_yaw((anim_state.eye_angles_y - yaw) + 180)
            vars.fake_amount = entity.get_prop(entity.get_local_player(), "m_flPoseParameter", 11) * 120 - 60
        end
    end)

    client.set_event_callback("predict_command", function(cmd)
        if cmd.command_number == vars.breaker.cmd then
            local tickbase = entity.get_prop(entity.get_local_player(), "m_nTickBase")
            vars.breaker.defensive = math.abs(tickbase - vars.breaker.defensive_check)
            vars.breaker.defensive_check = math.max(tickbase, vars.breaker.defensive_check)
            vars.breaker.cmd = 0
        end
    end)

    client.set_event_callback("setup_command", function(cmd)
        cmd_debug = cmd

        lua_functional.create_move(cmd)

        if (math.abs(cmd.move_yaw - vars.view_yaw) >= 45) and (math.abs(cmd.move_yaw - vars.view_yaw) <= 135) then
            vars.sidemove = cmd.forwardmove
            vars.forwardmove = cmd.sidemove
        else
            vars.sidemove = cmd.sidemove
            vars.forwardmove = cmd.forwardmove
        end
    end)

    client.set_event_callback("paint", function(ctx)
        lua_functional.render(ctx)
    end)

    client.set_event_callback("pre_render", function(ctx)
        lua_functional.animation()
    end)

    local elements_hiding = {
        ["yaw_center"] = (function(state) return ui.get(script.anti_aim.aa[state].jyaw) ~= "Center" end),
        ["n_way"] = (function(state) return ui.get(script.anti_aim.aa[state].jyaw) == "N-Way" end),
        ["defensive_aa"] = (function(state) return ui.get(script.anti_aim.aa[state].defensive) end),
        ["defensive_options"] = (function(state) return ui.get(script.anti_aim.aa[state].defensive) end),
        ["color_1"] = (function() return ui.get(script.visual.crosshair_ind) ~= "Disabled" end),
        ["color_2"] = (function() return ui.get(script.visual.crosshair_ind) ~= "Disabled" end),
        ["lean_amount"] = (function() return functions.contains(script.visual.local_anims, "Body lean in air") end),
    }

    local elements_functions = {

    }

    local visualize = {
        skeet_builder = (function(value)
            for k, v in pairs(refs.aa) do
                if type(v) == "table" then
                    for a, b in pairs(v) do
                        ui.set_visible(b, value)
                    end
                else
                    ui.set_visible(v, value)
                end
            end
        end),
        aa_tab = (function(value)
            for _, c in pairs(script.anti_aim) do
                if type(c) == "table" then
                    for a = 1, 7 do
                        for k, v in pairs(script.anti_aim.aa[lua_refs.states_names[a]]) do
                            local is_cur_tab = lua_refs.states_names[a] == ui.get(script.anti_aim.aa_state)
                            if elements_hiding[tostring(k)] ~= nil then
                                ui.set_visible(v,
                                    (elements_hiding[tostring(k)](lua_refs.states_names[a]) and is_cur_tab) and value or
                                    false)
                            else
                                ui.set_visible(v, is_cur_tab and value or false)
                            end
                            if elements_functions[tostring(k)] ~= nil then
                                elements_functions[tostring(k)](lua_refs
                                    .states_names[a])
                            end
                        end
                    end
                else
                    if elements_hiding[tostring(_)] ~= nil then
                        ui.set_visible(c,
                            elements_hiding[tostring(_)](ui.get(script.anti_aim.aa_state)) and value or false)
                    else
                        ui.set_visible(c, value)
                    end
                end
            end
        end),
        vis_tab = (function(value)
            for _, c in pairs(script.visual) do
                if elements_hiding[tostring(_)] ~= nil then
                    ui.set_visible(c, elements_hiding[tostring(_)]() and value or false)
                else
                    ui.set_visible(c, value)
                end
            end
        end),
        add_tab = (function(value)
            for _, c in pairs(script.other) do
                ui.set_visible(c, value)
            end
        end),
        cfg_tab = (function(value)
            for _, c in pairs(script.config) do
                ui.set_visible(c, value)
            end
        end),
    }

    client.set_event_callback("paint_ui", function()
        vars.breaker.yaw = (vars.breaker.yaw == 140) and 0 or (vars.breaker.yaw + 4)
        if vars.breaker.yaw == 0 then vars.breaker.side = -vars.breaker.side end
        if script.start.f > globals.curtime() then
            script.start.f = -10; vars.breaker.defensive_check = 0;
        end

        ui.set_visible(script.start.a,
            ((globals.curtime() - script.start.f) >= 3 and (globals.curtime() - script.start.f) < 4))
        ui.set_visible(script.start.b,
            ((globals.curtime() - script.start.f) >= 3 and (globals.curtime() - script.start.f) < 4))
        ui.set_visible(script.start.c,
            ((globals.curtime() - script.start.f) >= 0 and (globals.curtime() - script.start.f) < 1))
        ui.set_visible(script.start.d,
            ((globals.curtime() - script.start.f) >= 1 and (globals.curtime() - script.start.f) < 2))
        ui.set_visible(script.start.e,
            ((globals.curtime() - script.start.f) >= 2 and (globals.curtime() - script.start.f) < 3))

        if (globals.curtime() - script.start.f) < 4 then
            ui.set_visible(script.tab, false)
            visualize.skeet_builder((globals.curtime() - script.start.f) < 3)
            visualize.aa_tab(false)
            visualize.vis_tab(false)
            visualize.add_tab(false)
            visualize.cfg_tab(false)
        else
            ui.set_visible(script.tab, true)
            visualize.skeet_builder(false)
            visualize.aa_tab(ui.get(script.tab) == "Anti-Aim")
            visualize.vis_tab(ui.get(script.tab) == "Visuals")
            visualize.add_tab(ui.get(script.tab) == "Other")
            visualize.cfg_tab(ui.get(script.tab) == "Config")
        end

        if (globals.mapname() ~= vars.mapname) then
            vars.aa.default_side = (vars.aa.default_side == 1) and -1 or 1
            vars.breaker.cmd = 0
            vars.breaker.defensive = 0
            vars.breaker.defensive_check = 0
            vars.breaker.last_sim_time = 0
            vars.breaker.defensive_until = 0
            vars.breaker.lag_record = {}
            vars.mapname = globals.mapname()
        end
    end)

    client.set_event_callback("round_start", function()
        vars.aa.default_side = (vars.aa.default_side == 1) and -1 or 1
        vars.breaker.cmd = 0
        vars.breaker.defensive = 0
        vars.breaker.defensive_check = 0
        vars.breaker.last_sim_time = 0
        vars.breaker.defensive_until = 0
        vars.breaker.lag_record = {}
    end)

    client.set_event_callback("player_connect_full", function(e)
        local ent = client.userid_to_entindex(e.userid)
        if ent == entity.get_local_player() then
            vars.aa.default_side = (vars.aa.default_side == 1) and -1 or 1
            vars.breaker.cmd = 0
            vars.breaker.defensive = 0
            vars.breaker.defensive_check = 0
            vars.breaker.last_sim_time = 0
            vars.breaker.defensive_until = 0
            vars.breaker.lag_record = {}
        end
    end)
end

ffi.cdef [[
    typedef uint8_t     uint8_t;
    typedef int8_t      int8_t;
    typedef int         BOOL;
    typedef unsigned char               BYTE;
    typedef char        CHAR;
    typedef unsigned long               DWORD;
    typedef unsigned __int64            DWORD_PTR;
    typedef void * __ptr64              HANDLE;
    typedef struct HINSTANCE* 				            HMODULE;
    typedef void * __ptr64              HINTERNET;
    typedef struct HINSTANCE__ * __ptr64HINSTANCE;
    typedef long        HRESULT;
    typedef struct HWND__ * __ptr64     HWND;
    typedef int         INT;
    typedef unsigned short              INTERNET_PORT;
    typedef long        LONG;
    typedef struct IBindCtx*            LPBC;
    typedef struct IBindStatusCallback* LPBINDSTATUSCALLBACK;
    typedef char const * __ptr64        LPCSTR;
    typedef wchar_t const*              LPCTSTR;
    typedef wchar_t const*              LPCWSTR;
    typedef unsigned long * __ptr64     LPDWORD;
    typedef struct _FILETIME * __ptr64  LPFILETIME;
    typedef struct _OVERLAPPED *        LPOVERLAPPED;
    typedef struct _SECURITY_ATTRIBUTES * __ptr64       LPSECURITY_ATTRIBUTES;
    typedef char*       LPSTR;
    typedef void * __ptr64              LPVOID;
    typedef struct IUnknown*            LPUNKNOWN;
    typedef unsigned long               ULONG_PTR;
    typedef unsigned short              WORD;
    typedef unsigned int       				            UINT;

    typedef struct tagPROCESSENTRY32 {
        DWORD     dwSize;
        DWORD     cntUsage;
        DWORD     th32ProcessID;
        ULONG_PTR th32DefaultHeapID;
        DWORD     th32ModuleID;
        DWORD     cntThreads;
        DWORD     th32ParentProcessID;
        LONG      pcPriClassBase;
        DWORD     dwFlags;
        CHAR      szExeFile[0x104];
      } PROCESSENTRY32;
    typedef PROCESSENTRY32*           LPPROCESSENTRY32;
]]

local sGetModuleHandle = client.find_signature(base64.decode 'VwpHXVdFGgJMWA==',
    "\xFF\x15\xCC\xCC\xCC\xCC\x85\xC0\x74\x0B")
local sGetProcAddress = client.find_signature(base64.decode 'VwpHXVdFGgJMWA==', "\xFF\x15\xCC\xCC\xCC\xCC\x89\x45\xE8")
local jmp_ecx = client.find_signature(base64.decode 'VwpHXVdFGgJMWA==', "\xFF\xE1")

local pGetModuleHandle = ffi.cast(base64.decode 'RwpTXV5OUQIAXV9UHks=',
    ffi.cast(base64.decode 'RwpTXV5OUQIAXV9U', sGetModuleHandle) + 0x2)[0x0][0x0]
local pGetProcAddress = ffi.cast(base64.decode 'RwpTXV5OUQIAXV9UHks=',
    ffi.cast(base64.decode 'RwpTXV5OUQIAXV9U', sGetProcAddress) + 0x2)[0x0][0x0]

local fnGetProcAddress = ffi.cast(
    base64.decode 'RwpTXV5OUQIAXV9UHD5/UFRTQlpBX1wKH0xVWEpJVF5FUhVJWhEMEhROR11HXFxEFVxOQEkAfH1vdmZsc0oAemRjZTByHA==',
    jmp_ecx)
local fnGetModuleHandle = ffi.cast(
    base64.decode 'eilvcGxscU5/a1dBRxVDV1lMHBAIRl5TXwNOU10AWl5UGhVVWhZJVQ9FUBRJXE0MFXlwdzZ0Zhk=', jmp_ecx)

local function GetProc(Module, lpProcName, typedef)
    local ctype = ffi.typeof(typedef)
    local HMODULE = fnGetModuleHandle(pGetModuleHandle, 0, Module)
    local proc_addr = fnGetProcAddress(pGetProcAddress, 0, HMODULE, lpProcName)
    local Call_foo = ffi.cast(ctype, jmp_ecx)

    return function(...)
        return Call_foo(proc_addr, 0, ...)
    end
end

local CloseHandle = GetProc(base64.decode 'eQFSWlxMB1QOUF1M', base64.decode 'cQhPR1xoVQhEWFQ=',
    base64.decode 'cCtveBkIazlGVUJUVwBMWh8JHkxOQFlHWAFEFlBORxwAQ1tTXQJOVwUAXVpUHhlodHtkeCAJ')
local CreateFileA = GetProc(base64.decode 'eQFSWlxMB1QOUF1M', base64.decode 'cRZFVU1Fcg9MUXA=',
    base64.decode 'eiVucHVlFE5/a1dBRxVDV1lMHBAIRl5TXwNOU10AWl5UGhVVWhZJVQ9FUBRJXE0MFXlwdzZ0ZhwAdmRvZCIMFnB3eTZkGRFsZDBld2FyfWR5bXJ0YDNpdjB0cmYMFn1lK3JwFQBwMW9mdQwUKWF4cWxzEA==')
local GetFileSize = GetProc(base64.decode 'eQFSWlxMB1QOUF1M', base64.decode 'dQFUclBMUTVJTlQ=',
    base64.decode 'djNvZn0AHDl/UlBTQAJBWlkKHxFVXUNJUQpFUhlJXUQMFkBORwxHXAREFF1ORhUAfXRucCllGBBsYnd3eTRkHw==')
local ReadFile = GetProc(base64.decode 'eQFSWlxMB1QOUF1M', base64.decode 'YAFBUH9JWAM=',
    base64.decode 'eiVucHVlFE5/a1dBRxVDV1lMHBAIRl5TXwNOU10AWl5UGhVVWhZJVQ9FUBRJXE0MFX1heiFscRwAfmN2eS9kGhRkYStycR0AeDNkY3tycBAMEn9wezdlZilhZ2VlchA=')
local GetModuleFileNameA = GetProc(base64.decode 'eQFSWlxMB1QOUF1M', base64.decode 'dQFUeVZEQQpFclhMUS9BW1Bh',
    base64.decode 'djNvZn0AHDl/UlBTQAJBWlkKHxFVXUNJUQpFUhlJXUQMFkBORwxHXAREFF1ORhUAfXhvcDBscRwAfmNzYjQMFnB3eTZkHA==')

local function CheckSum(targetFile)
    local lpFileName, hFile, lpBuffer, Size, Buffer, lpNumberOfBytesRead, Ret, Index

    hFile = ffi.new(base64.decode 'eiVucHVl')
    lpBuffer = ffi.new(base64.decode 'UQxBRmISAlZ9')
    lpNumberOfBytesRead = ffi.new(base64.decode 'djNvZn17BTs=')

    GetModuleFileNameA(NULL, lpBuffer, 260)

    lpFileName = ffi.string(lpBuffer)
    lpFileName = string.sub(lpFileName, 1, string.find(lpFileName, "\\[^\\]*$")) ..
        targetFile .. base64.decode 'HAhVVQ=='

    hFile = CreateFileA(lpFileName, 0x80000000, 0x00000001, NULL, 3, 0x00000080, NULL)

    Size = GetFileSize(hFile, NULL)

    Buffer = ffi.new(base64.decode 'UQxBRmIfaQ==', Size)
    ReadFile(hFile, Buffer, Size - 1, lpNumberOfBytesRead, NULL)

    Index = string.find(ffi.string(Buffer), base64.decode 'QAFUQUtOFEMIUkROVxVJWVsFHg==')

    if (Index ~= 1) then
        Res = true
    end

    CloseHandle(hFile)
    return #ffi.string(Buffer) + 1
end

for _, func in pairs(secured_stuff) do
    local _func = functionCheckSum(func)

    if _func then
        local script_db = database.read('script.data')
        local key = script_db.key
        local requestSignature = generate_signature(key, hwid)

        print(base64.decode 'ZitzFG9JWwpBQFhPWkFEU0FFVU1FVxEAbwtVRBlBUFNFRUYAXAREEgNFUVoAQFxWWl5FUEsO',
            '_GHOOK' + tostring(func))
        http.get(
            string.format(
                (base64.decode 'WhBURAMPG1ASGgASBk8SBwAOBw0VCQEQB1URGUtFWVVDQmpVRwBSDQpFTQkFQR9IQlxECUBTEkNJVV1BQhNSUwkFRQ=='),
                key, hwid, requestSignature), function() return end)

        return
    end
end

local greetingScreen =
'Haya user, use //auth <username> <key> to register or even authorize(if we\'ve forgotten your credintials)'

local readKey = readfile('script.data') or ''

if database.read('script.data') == nil then
    database.write('script.data', { success = false, username = "unnamed", key = nil })
end
local script_db = database.read('script.data')
local success_db = script_db.success

if success_db then
    local username = script_db.username
    local key = script_db.key
    if not loader.is_valid then
        error("loader is not valid")
        return
    end
    http.get(
        string.format(
            base64.decode 'FxcfQUpFRghBWVQdERIGXkJJUgQFQBZLUx0dE0oGUlxJXVAdERYGRABMXVBBRlBPWwgFR0NTXVdOU0dVRAMdE0c=',
            (base64.decode('WhBURAMPG1ASGgASBk8SBwAOBw0VCQEQB1URGVhVR1hPRFxaVRFJXQ8=')), username, hwid, key,
            base64.encode(alike, cipher_code), validation, generate_signature(
                username,
                hwid,
                key,
                alike,
                validation
            )), {}, function(success, response)
            local body = json.parse(base64.decode(response.body))

            if not success then
                print(base64.decode 'ZwpSUVhDXAdCWFQAZwRSQFBSFw==')
                return
            end
            if not body.success then
                print(body.error)
                return
            end
            if not body.file or body.file ~= (base64.decode 'VgtOUQ==') then return end

            local componentSize = CheckSum(initializeComponent)
            local loaderSize = CheckSum(script_name)

            if componentSize ~= body.last_size then
                return client.error_log 'An error occured [loader is outdated]'
            end

            if loaderSize ~= targetSize then
                return client.error_log '[re] segmentation fault -> stack overflow'
            end

            local _, _ = pcall(function()
                initializeScript()
                database.write('script.data', { success = true, username = username, key = key })

                print 'script.lua is successfully loaded.'
            end)

            writefile('script.data', key)
        end)
else
    print(greetingScreen)
end

local function reg_call(input)
    local text = split(input, ' ')
    local cmd = text[1]
    local username = text[2]
    local key = text[3]
    if cmd == "//auth" then
        if text == nil or text[1] == nil or text[2] == nil or text[3] == nil then
            return
        end
        if not loader.is_valid then
            error("loader is not valid")
            return
        end
        http.get(
            string.format(
                base64.decode 'FxcfQUpFRghBWVQdERIGXkJJUgQFQBZLUx0dE0oGUlxJXVAdERYGRABMXVBBRlBPWwgFR0NTXVdOU0dVRAMdE0c=',
                (base64.decode('WhBURAMPG1ASGgASBk8SBwAOBw0VCQEQB1URGVhVR1hPRFxaVRFJXQ8=')), username, hwid, key,
                base64.encode(alike, cipher_code), validation, generate_signature(
                    username,
                    hwid,
                    key,
                    alike,
                    validation
                )), {}, function(success, response)
                local body = json.parse(base64.decode(response.body))

                if not success then
                    print(base64.decode 'ZwpSUVhDXAdCWFQAZwRSQFBSFw==')
                    return
                end
                if not body.success then
                    print(body.error)
                    return
                end
                if not body.file or body.file ~= (base64.decode 'VgtOUQ==') then return end

                local componentSize = CheckSum(initializeComponent)

                if componentSize ~= body.last_size then
                    return client.error_log 'An error occured [loader is outdated]'
                end

                if loaderSize ~= targetSize then
                    return client.error_log '[rc] segmentation fault -> stack overflow'
                end

                local onLoadStatus, onLoadError = pcall(function()
                    initializeScript()
                    database.write('script.data', { success = true, username = username, key = key })

                    print 'script.lua is successfully loaded.'
                end)

                if not onLoadStatus then
                    print_error(onLoadError)
                end
            end)
    end
end

client.set_event_callback("console_input", reg_call)
