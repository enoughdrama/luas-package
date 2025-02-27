
--region angle
--- @class angle_old_c
--- @field public p number Angle pitch.
--- @field public y number Angle yaw.
--- @field public r number Angle roll.
local angle_old_c = {}
local angle_mt = {
	__index = angle_old_c
}

--- Overwrite the angle's angles. Nil values leave the angle unchanged.
--- @param angle angle_old_c
--- @param p_new number
--- @param y_new number
--- @param r_new number
--- @return void
angle_mt.__call = function(angle, p_new, y_new, r_new)
	p_new = p_new or angle.p
	y_new = y_new or angle.y
	r_new = r_new or angle.r

	angle.p = p_new
	angle.y = y_new
	angle.r = r_new
end

--- Create a new vector object.
--- @param p number
--- @param y number
--- @param r number
--- @return angle_old_c
local function angle(p, y, r)
	return setmetatable(
		{
			p = p and p or 0,
			y = y and y or 0,
			r = r and r or 0
		},
		angle_mt
	)
end

--- Offset the angle's angles. Nil values leave the angle unchanged.
--- @param p_offset number
--- @param y_offset number
--- @param r_offset number
--- @return void
function angle_old_c:offset(p_offset, y_offset, r_offset)
	p_offset = self.p + p_offset or 0
	y_offset = self.y + y_offset or 0
	r_offset = self.r + r_offset or 0

	self.p = self.p + p_offset
	self.y = self.y + y_offset
	self.r = self.r + r_offset
end

--- Clone the angle object.
--- @return angle_old_c
function angle_old_c:clone()
	return setmetatable(
		{
			p = self.p,
			y = self.y,
			r = self.r
		},
		angle_mt
	)
end

--- Clone and offset the angle's angles. Nil values leave the angle unchanged.
--- @param p_offset number
--- @param y_offset number
--- @param r_offset number
--- @return angle_old_c
function angle_old_c:clone_offset(p_offset, y_offset, r_offset)
	p_offset = self.p + p_offset or 0
	y_offset = self.y + y_offset or 0
	r_offset = self.r + r_offset or 0

	return angle(
		self.p + p_offset,
		self.y + y_offset,
		self.r + r_offset
	)
end

--- Unpack the angle.
--- @return number, number, number
function angle_old_c:unpack()
	return self.p, self.y, self.r
end

--- Set the angle's euler angles to 0.
--- @return void
function angle_old_c:nullify()
	self.p = 0
	self.y = 0
	self.r = 0
end

--- Returns a string representation of the angle.
function angle_mt.__tostring(operand_a)
	return string.format("%s, %s, %s", operand_a.p, operand_a.y, operand_a.r)
end

--- Concatenates the angle in a string.
function angle_mt.__concat(operand_a)
	return string.format("%s, %s, %s", operand_a.p, operand_a.y, operand_a.r)
end

--- Adds the angle to another angle.
function angle_mt.__add(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return angle(
			operand_a + operand_b.p,
			operand_a + operand_b.y,
			operand_a + operand_b.r
		)
	end

	if (type(operand_b) == "number") then
		return angle(
			operand_a.p + operand_b,
			operand_a.y + operand_b,
			operand_a.r + operand_b
		)
	end

	return angle(
		operand_a.p + operand_b.p,
		operand_a.y + operand_b.y,
		operand_a.r + operand_b.r
	)
end

--- Subtracts the angle from another angle.
function angle_mt.__sub(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return angle(
			operand_a - operand_b.p,
			operand_a - operand_b.y,
			operand_a - operand_b.r
		)
	end

	if (type(operand_b) == "number") then
		return angle(
			operand_a.p - operand_b,
			operand_a.y - operand_b,
			operand_a.r - operand_b
		)
	end

	return angle(
		operand_a.p - operand_b.p,
		operand_a.y - operand_b.y,
		operand_a.r - operand_b.r
	)
end

--- Multiplies the angle with another angle.
function angle_mt.__mul(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return angle(
			operand_a * operand_b.p,
			operand_a * operand_b.y,
			operand_a * operand_b.r
		)
	end

	if (type(operand_b) == "number") then
		return angle(
			operand_a.p * operand_b,
			operand_a.y * operand_b,
			operand_a.r * operand_b
		)
	end

	return angle(
		operand_a.p * operand_b.p,
		operand_a.y * operand_b.y,
		operand_a.r * operand_b.r
	)
end

--- Divides the angle by the another angle.
function angle_mt.__div(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return angle(
			operand_a / operand_b.p,
			operand_a / operand_b.y,
			operand_a / operand_b.r
		)
	end

	if (type(operand_b) == "number") then
		return angle(
			operand_a.p / operand_b,
			operand_a.y / operand_b,
			operand_a.r / operand_b
		)
	end

	return angle(
		operand_a.p / operand_b.p,
		operand_a.y / operand_b.y,
		operand_a.r / operand_b.r
	)
end

--- Raises the angle to the power of an another angle.
function angle_mt.__pow(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return angle(
			math.pow(operand_a, operand_b.p),
			math.pow(operand_a, operand_b.y),
			math.pow(operand_a, operand_b.r)
		)
	end

	if (type(operand_b) == "number") then
		return angle(
			math.pow(operand_a.p, operand_b),
			math.pow(operand_a.y, operand_b),
			math.pow(operand_a.r, operand_b)
		)
	end

	return angle(
		math.pow(operand_a.p, operand_b.p),
		math.pow(operand_a.y, operand_b.y),
		math.pow(operand_a.r, operand_b.r)
	)
end

--- Performs modulo on the angle with another angle.
function angle_mt.__mod(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return angle(
			operand_a % operand_b.p,
			operand_a % operand_b.y,
			operand_a % operand_b.r
		)
	end

	if (type(operand_b) == "number") then
		return angle(
			operand_a.p % operand_b,
			operand_a.y % operand_b,
			operand_a.r % operand_b
		)
	end

	return angle(
		operand_a.p % operand_b.p,
		operand_a.y % operand_b.y,
		operand_a.r % operand_b.r
	)
end

--- Perform a unary minus operation on the angle.
function angle_mt.__unm(operand_a)
	return angle(
		-operand_a.p,
		-operand_a.y,
		-operand_a.r
	)
end

--- Clamps the angle's angles to whole numbers. Equivalent to "angle:round" with no precision.
--- @return void
function angle_old_c:round_zero()
	self.p = math.floor(self.p + 0.5)
	self.y = math.floor(self.y + 0.5)
	self.r = math.floor(self.r + 0.5)
end

--- Round the angle's angles.
--- @param precision number
function angle_old_c:round(precision)
	self.p = math.round(self.p, precision)
	self.y = math.round(self.y, precision)
	self.r = math.round(self.r, precision)
end

--- Clamps the angle's angles to the nearest base.
--- @param base number
function angle_old_c:round_base(base)
	self.p = base * math.round(self.p / base)
	self.y = base * math.round(self.y / base)
	self.r = base * math.round(self.r / base)
end

--- Clamps the angle's angles to whole numbers. Equivalent to "angle:round" with no precision.
--- @return angle_old_c
function angle_old_c:rounded_zero()
	return angle(
		math.floor(self.p + 0.5),
		math.floor(self.y + 0.5),
		math.floor(self.r + 0.5)
	)
end

--- Round the angle's angles.
--- @param precision number
--- @return angle_old_c
function angle_old_c:rounded(precision)
	return angle(
		math.round(self.p, precision),
		math.round(self.y, precision),
		math.round(self.r, precision)
	)
end

--- Clamps the angle's angles to the nearest base.
--- @param base number
--- @return angle_old_c
function angle_old_c:rounded_base(base)
	return angle(
		base * math.round(self.p / base),
		base * math.round(self.y / base),
		base * math.round(self.r / base)
	)
end
--endregion

--region vector
--- @class vector_old_c
--- @field public x number X coordinate.
--- @field public y number Y coordinate.
--- @field public z number Z coordinate.
local vector_old_c = {}
local vector_mt = {
	__index = vector_old_c,
}

--- Overwrite the vector's coordinates. Nil will leave coordinates unchanged.
--- @param x_new number
--- @param y_new number
--- @param z_new number
--- @return void
vector_mt.__call = function(vector, x_new, y_new, z_new)
	x_new = x_new or vector.x
	y_new = y_new or vector.y
	z_new = z_new or vector.z

	vector.x = x_new
	vector.y = y_new
	vector.z = z_new
end

--- Create a new vector object.
--- @param x number
--- @param y number
--- @param z number
--- @return vector_old_c
local function vector(x, y, z)
	return setmetatable(
		{
			x = x and x or 0,
			y = y and y or 0,
			z = z and z or 0
		},
		vector_mt
	)
end

--- Offset the vector's coordinates. Nil will leave the coordinates unchanged.
--- @param x_offset number
--- @param y_offset number
--- @param z_offset number
--- @return void
function vector_old_c:offset(x_offset, y_offset, z_offset)
	x_offset = x_offset or 0
	y_offset = y_offset or 0
	z_offset = z_offset or 0

	self.x = self.x + x_offset
	self.y = self.y + y_offset
	self.z = self.z + z_offset
end

--- Clone the vector object.
--- @return vector_old_c
function vector_old_c:clone()
	return setmetatable(
		{
			x = self.x,
			y = self.y,
			z = self.z
		},
		vector_mt
	)
end

--- Clone the vector object and offset its coordinates. Nil will leave the coordinates unchanged.
--- @param x_offset number
--- @param y_offset number
--- @param z_offset number
--- @return vector_old_c
function vector_old_c:clone_offset(x_offset, y_offset, z_offset)
	x_offset = x_offset or 0
	y_offset = y_offset or 0
	z_offset = z_offset or 0

	return setmetatable(
		{
			x = self.x + x_offset,
			y = self.y + y_offset,
			z = self.z + z_offset
		},
		vector_mt
	)
end

--- Unpack the vector.
--- @return number, number, number
function vector_old_c:unpack()
	return self.x, self.y, self.z
end

--- Set the vector's coordinates to 0.
--- @return void
function vector_old_c:nullify()
	self.x = 0
	self.y = 0
	self.z = 0
end

--- Returns a string representation of the vector.
function vector_mt.__tostring(operand_a)
	return string.format("%s, %s, %s", operand_a.x, operand_a.y, operand_a.z)
end

--- Concatenates the vector in a string.
function vector_mt.__concat(operand_a)
	return string.format("%s, %s, %s", operand_a.x, operand_a.y, operand_a.z)
end


--- Returns true if the vector's coordinates are equal to another vector.
function vector_mt.__eq(operand_a, operand_b)
	return (operand_a.x == operand_b.x) and (operand_a.y == operand_b.y) and (operand_a.z == operand_b.z)
end

--- Returns true if the vector is less than another vector.
function vector_mt.__lt(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return (operand_a < operand_b.x) or (operand_a < operand_b.y) or (operand_a < operand_b.z)
	end

	if (type(operand_b) == "number") then
		return (operand_a.x < operand_b) or (operand_a.y < operand_b) or (operand_a.z < operand_b)
	end

	return (operand_a.x < operand_b.x) or (operand_a.y < operand_b.y) or (operand_a.z < operand_b.z)
end

--- Returns true if the vector is less than or equal to another vector.
function vector_mt.__le(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return (operand_a <= operand_b.x) or (operand_a <= operand_b.y) or (operand_a <= operand_b.z)
	end

	if (type(operand_b) == "number") then
		return (operand_a.x <= operand_b) or (operand_a.y <= operand_b) or (operand_a.z <= operand_b)
	end

	return (operand_a.x <= operand_b.x) or (operand_a.y <= operand_b.y) or (operand_a.z <= operand_b.z)
end

--- Add a vector to another vector.
function vector_mt.__add(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return vector(
			operand_a + operand_b.x,
			operand_a + operand_b.y,
			operand_a + operand_b.z
		)
	end

	if (type(operand_b) == "number") then
		return vector(
			operand_a.x + operand_b,
			operand_a.y + operand_b,
			operand_a.z + operand_b
		)
	end

	return vector(
		operand_a.x + operand_b.x,
		operand_a.y + operand_b.y,
		operand_a.z + operand_b.z
	)
end

--- Subtract a vector from another vector.
function vector_mt.__sub(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return vector(
			operand_a - operand_b.x,
			operand_a - operand_b.y,
			operand_a - operand_b.z
		)
	end

	if (type(operand_b) == "number") then
		return vector(
			operand_a.x - operand_b,
			operand_a.y - operand_b,
			operand_a.z - operand_b
		)
	end

	return vector(
		operand_a.x - operand_b.x,
		operand_a.y - operand_b.y,
		operand_a.z - operand_b.z
	)
end

--- Multiply a vector with another vector.
function vector_mt.__mul(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return vector(
			operand_a * operand_b.x,
			operand_a * operand_b.y,
			operand_a * operand_b.z
		)
	end

	if (type(operand_b) == "number") then
		return vector(
			operand_a.x * operand_b,
			operand_a.y * operand_b,
			operand_a.z * operand_b
		)
	end

	return vector(
		operand_a.x * operand_b.x,
		operand_a.y * operand_b.y,
		operand_a.z * operand_b.z
	)
end

--- Divide a vector by another vector.
function vector_mt.__div(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return vector(
			operand_a / operand_b.x,
			operand_a / operand_b.y,
			operand_a / operand_b.z
		)
	end

	if (type(operand_b) == "number") then
		return vector(
			operand_a.x / operand_b,
			operand_a.y / operand_b,
			operand_a.z / operand_b
		)
	end

	return vector(
		operand_a.x / operand_b.x,
		operand_a.y / operand_b.y,
		operand_a.z / operand_b.z
	)
end

--- Raised a vector to the power of another vector.
function vector_mt.__pow(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return vector(
			math.pow(operand_a, operand_b.x),
			math.pow(operand_a, operand_b.y),
			math.pow(operand_a, operand_b.z)
		)
	end

	if (type(operand_b) == "number") then
		return vector(
			math.pow(operand_a.x, operand_b),
			math.pow(operand_a.y, operand_b),
			math.pow(operand_a.z, operand_b)
		)
	end

	return vector(
		math.pow(operand_a.x, operand_b.x),
		math.pow(operand_a.y, operand_b.y),
		math.pow(operand_a.z, operand_b.z)
	)
end

--- Performs a modulo operation on a vector with another vector.
function vector_mt.__mod(operand_a, operand_b)
	if (type(operand_a) == "number") then
		return vector(
			operand_a % operand_b.x,
			operand_a % operand_b.y,
			operand_a % operand_b.z
		)
	end

	if (type(operand_b) == "number") then
		return vector(
			operand_a.x % operand_b,
			operand_a.y % operand_b,
			operand_a.z % operand_b
		)
	end

	return vector(
		operand_a.x % operand_b.x,
		operand_a.y % operand_b.y,
		operand_a.z % operand_b.z
	)
end

--- Perform a unary minus operation on the vector.
function vector_mt.__unm(operand_a)
	return vector(
		-operand_a.x,
		-operand_a.y,
		-operand_a.z
	)
end

--- Returns the vector's 2 dimensional length squared.
--- @return number
function vector_old_c:length2_squared()
	return (self.x * self.x) + (self.y * self.y);
end

--- Return's the vector's 2 dimensional length.
--- @return number
function vector_old_c:length2()
	return math.sqrt(self:length2_squared())
end

--- Returns the vector's 3 dimensional length squared.
--- @return number
function vector_old_c:length_squared()
	return (self.x * self.x) + (self.y * self.y) + (self.z * self.z);
end

--- Return's the vector's 3 dimensional length.
--- @return number
function vector_old_c:length()
	return math.sqrt(self:length_squared())
end

--- Returns the vector's dot product.
--- @param other_vector vector_old_c
--- @return number
function vector_old_c:dot_product(other_vector)
	return (self.x * other_vector.x) + (self.y * other_vector.y) + (self.z * other_vector.z)
end

--- Returns the vector's cross product.
--- @param other_vector vector_old_c
--- @return vector_old_c
function vector_old_c:cross_product(other_vector)
	return vector_old_c(
		(self.y * other_vector.z) - (self.z * other_vector.y),
		(self.z * other_vector.x) - (self.x * other_vector.z),
		(self.x * other_vector.y) - (self.y * other_vector.x)
	)
end

--- Returns the 2 dimensional distance between the vector and another vector.
--- @param other_vector vector_old_c
--- @return number
function vector_old_c:distance2(other_vector)
	return (other_vector - self):length2()
end

--- Returns the 3 dimensional distance between the vector and another vector.
--- @param other_vector vector_old_c
--- @return number
function vector_old_c:distance(other_vector)
	return (other_vector - self):length()
end

--- Returns the distance on the X axis between the vector and another vector.
--- @param other_vector vector_old_c
--- @return number
function vector_old_c:distance_x(other_vector)
	return math.abs(self.x - other_vector.x)
end

--- Returns the distance on the Y axis between the vector and another vector.
--- @param other_vector vector_old_c
--- @return number
function vector_old_c:distance_y(other_vector)
	return math.abs(self.y - other_vector.y)
end

--- Returns the distance on the Z axis between the vector and another vector.
--- @param other_vector vector_old_c
--- @return number
function vector_old_c:distance_z(other_vector)
	return math.abs(self.z - other_vector.z)
end

--- Returns true if the vector is within the given distance to another vector.
--- @param other_vector vector_old_c
--- @param distance number
--- @return boolean
function vector_old_c:in_range(other_vector, distance)
	return self:distance(other_vector) <= distance
end

--- Clamps the vector's coordinates to whole numbers. Equivalent to "vector:round" with no precision.
--- @return void
function vector_old_c:round_zero()
	self.x = math.floor(self.x + 0.5)
	self.y = math.floor(self.y + 0.5)
	self.z = math.floor(self.z + 0.5)
end

--- Round the vector's coordinates.
--- @param precision number
--- @return void
function vector_old_c:round(precision)
	self.x = math.round(self.x, precision)
	self.y = math.round(self.y, precision)
	self.z = math.round(self.z, precision)
end

--- Clamps the vector's coordinates to the nearest base.
--- @param base number
--- @return void
function vector_old_c:round_base(base)
	self.x = base * math.round(self.x / base)
	self.y = base * math.round(self.y / base)
	self.z = base * math.round(self.z / base)
end

--- Clamps the vector's coordinates to whole numbers. Equivalent to "vector:round" with no precision.
--- @return vector_old_c
function vector_old_c:rounded_zero()
	return vector(
		math.floor(self.x + 0.5),
		math.floor(self.y + 0.5),
		math.floor(self.z + 0.5)
	)
end

--- Round the vector's coordinates.
--- @param precision number
--- @return vector_old_c
function vector_old_c:rounded(precision)
	return vector(
		math.round(self.x, precision),
		math.round(self.y, precision),
		math.round(self.z, precision)
	)
end

--- Clamps the vector's coordinates to the nearest base.
--- @param base number
--- @return vector_old_c
function vector_old_c:rounded_base(base)
	return vector(
		base * math.round(self.x / base),
		base * math.round(self.y / base),
		base * math.round(self.z / base)
	)
end

--- Normalize the vector.
--- @return void
function vector_old_c:normalize()
	local length = self:length()

	-- Prevent possible divide-by-zero errors.
	if (length ~= 0) then
		self.x = self.x / length
		self.y = self.y / length
		self.z = self.z / length
	else
		self.x = 0
		self.y = 0
		self.z = 1
	end
end

--- Returns the normalized length of a vector.
--- @return number
function vector_old_c:normalized_length()
	return self:length()
end

--- Returns a copy of the vector, normalized.
--- @return vector_old_c
function vector_old_c:normalized()
	local length = self:length()

	if (length ~= 0) then
		return vector(
			self.x / length,
			self.y / length,
			self.z / length
		)
	else
		return vector(0, 0, 1)
	end
end

--- Returns a new 2 dimensional vector of the original vector when mapped to the screen, or nil if the vector is off-screen.
--- @return vector_old_c
function vector_old_c:to_screen()
	local x, y = renderer.world_to_screen(self.x, self.y, self.z)

	if (x == nil or y == nil) then
		return nil
	end

	return vector(x, y)
end

--- Returns the magnitude of the vector, use this to determine the speed of the vector if it's a velocity vector.
--- @return number
function vector_old_c:magnitude()
	return math.sqrt(
		math.pow(self.x, 2) +
			math.pow(self.y, 2) +
			math.pow(self.z, 2)
	)
end

--- Returns the angle of the vector in regards to another vector.
--- @param destination vector_old_c
--- @return angle_old_c
function vector_old_c:angle_to(destination)
	-- Calculate the delta of vectors.
	local delta_vector = vector(destination.x - self.x, destination.y - self.y, destination.z - self.z)

	if (delta_vector.x == 0 and delta_vector.y == 0) then
		return angle((delta_vector.z > 0 and 270 or 90), 0)
	else
		-- Calculate the yaw.
		local yaw = math.deg(math.atan2(delta_vector.y, delta_vector.x))

		-- Calculate the pitch.
		local hyp = math.sqrt(delta_vector.x * delta_vector.x + delta_vector.y * delta_vector.y)
		local pitch = math.deg(math.atan2(-delta_vector.z, hyp))

		return angle(pitch, yaw)
	end
end

--- Returns the result of client.trace_line between two vectors.
--- @param destination vector_old_c
--- @param skip_entindex number
--- @return number, number|nil
function vector_old_c:trace_line_to(destination, skip_entindex)
	skip_entindex = skip_entindex or -1

	return client.trace_line(
		skip_entindex,
		self.x,
		self.y,
		self.z,
		destination.x,
		destination.y,
		destination.z
	)
end

--- Lerp to another vector.
--- @param target vector_old_c
--- @param percentage number
--- @return vector_old_c
function vector_old_c:lerp(target, percentage)
	return self + (target - self) * percentage
end

--- Trace line to another vector and return the impact point.
--- @param destination vector_old_c
--- @param skip_entindex number
--- @return number, number, vector_old_c
function vector_old_c:trace_line_impact(destination, skip_entindex)
	skip_entindex = skip_entindex or -1

	local fraction, eid = client.trace_line(skip_entindex, self.x, self.y, self.z, destination.x, destination.y, destination.z)
	local impact = self:lerp(destination, fraction)

	return fraction, eid, impact
end

--- Trace line to another vector, skipping any entity indices returned by the callback.
--- @param destination vector_old_c
--- @param callback fun(eid: number): boolean
--- @param max_traces number
--- @return number, number, vector_old_c
function vector_old_c:trace_line_skip(destination, callback, max_traces)
	max_traces = max_traces or 10

	local fraction, eid = 0, -1
	local impact = self
	local i = 0

	while (max_traces >= i and fraction < 1 and ((eid > -1 and callback(eid)) or impact == self)) do
		fraction, eid, impact = impact:trace_line_impact(destination, eid)
		i = i + 1
	end

	return self:distance(impact) / self:distance(destination), eid, impact
end

--- Returns the result of client.trace_bullet between two vectors.
--- @param from_player number
--- @param destination vector_old_c
--- @return number|nil, number
function vector_old_c:trace_bullet_to(from_player, destination)
	return client.trace_bullet(
		from_player,
		self.x,
		self.y,
		self.z,
		destination.x,
		destination.y,
		destination.z
	)
end

--- Returns the vector of the closest point along a ray.
--- @param source vector_old_c
--- @param destination vector_old_c
--- @return vector_old_c
function vector_old_c:closest_ray_point(source, destination)
	local direction = (destination - source) / source:distance(destination)
	local v = self - source
	local length = v:dot_product(direction)

	return source + direction * length
end

--- Returns a point along a ray after dividing it.
--- @param destination vector_old_c
--- @param ratio number
--- @return vector_old_c
function vector_old_c:divide_ray(destination, ratio)
	return (self * ratio + destination) / (1 + ratio)
end

--- Internally divide a ray.
--- @param source vector_old_c
--- @param destination vector_old_c
--- @param m number
--- @param n number
--- @return vector_old_c
local function vector_internal_division(source, destination, m, n)
	return vector((source.x*n + destination.x*m) / (m+n),
		(source.y*n + destination.y*m) / (m+n),
		(source.z*n + destination.z*m) / (m+n))
end

--- Returns a ray divided into a number of segments.
--- @param destination vector_old_c
--- @param segments number
--- @return table<number, vector_old_c>
function vector_old_c:segment_ray(destination, segments)
	local points = {}

	for i = 0, segments do
		points[i] = vector_internal_division(self, destination, i, segments - i)
	end

	return points
end

--- Returns the best source vector and destination vector to draw a line on-screen using world-to-screen.
--- @param destination vector_old_c
--- @param total_segments number
--- @return vector_old_c|nil, vector_old_c|nil
function vector_old_c:ray(destination, total_segments)
	total_segments = total_segments or 128

	local segments = {}
	local step = self:distance(destination) / total_segments
	local angle = self:angle_to(destination)
	local direction = angle:to_forward_vector()

	for i = 1, total_segments do
		table.insert(segments, self + (direction * (step * i)))
	end

	local src_screen_position = vector(0, 0, 0)
	local dst_screen_position = vector(0, 0, 0)
	local src_in_screen = false
	local dst_in_screen = false

	for i = 1, #segments do
		src_screen_position = segments[i]:to_screen()

		if src_screen_position ~= nil then
			src_in_screen = true

			break
		end
	end

	for i = #segments, 1, -1 do
		dst_screen_position = segments[i]:to_screen()

		if dst_screen_position ~= nil then
			dst_in_screen = true

			break
		end
	end

	if src_in_screen and dst_in_screen then
		return src_screen_position, dst_screen_position
	end

	return nil
end
--endregion

--region angle_vector_methods
--- Returns a forward vector of the angle. Use this to convert an angle into a cartesian direction.
--- @return vector_old_c
function angle_old_c:to_forward_vector()
	local degrees_to_radians = function(degrees) return degrees * math.pi / 180 end

	local sp = math.sin(degrees_to_radians(self.p))
	local cp = math.cos(degrees_to_radians(self.p))
	local sy = math.sin(degrees_to_radians(self.y))
	local cy = math.cos(degrees_to_radians(self.y))

	return vector(cp * cy, cp * sy, -sp)
end

--- Return an up vector of the angle. Use this to convert an angle into a cartesian direction.
--- @return vector_old_c
function angle_old_c:to_up_vector()
	local degrees_to_radians = function(degrees) return degrees * math.pi / 180 end

	local sp = math.sin(degrees_to_radians(self.p))
	local cp = math.cos(degrees_to_radians(self.p))
	local sy = math.sin(degrees_to_radians(self.y))
	local cy = math.cos(degrees_to_radians(self.y))
	local sr = math.sin(degrees_to_radians(self.r))
	local cr = math.cos(degrees_to_radians(self.r))

	return vector(cr * sp * cy + sr * sy, cr * sp * sy + sr * cy * -1, cr * cp)
end

--- Return a right vector of the angle. Use this to convert an angle into a cartesian direction.
--- @return vector_old_c
function angle_old_c:to_right_vector()
	local degrees_to_radians = function(degrees) return degrees * math.pi / 180 end

	local sp = math.sin(degrees_to_radians(self.p))
	local cp = math.cos(degrees_to_radians(self.p))
	local sy = math.sin(degrees_to_radians(self.y))
	local cy = math.cos(degrees_to_radians(self.y))
	local sr = math.sin(degrees_to_radians(self.r))
	local cr = math.cos(degrees_to_radians(self.r))

	return vector(sr * sp * cy * -1 + cr * sy, sr * sp * sy * -1 + -1 * cr * cy, -1 * sr * cp)
end

--- Return a backward vector of the angle. Use this to convert an angle into a cartesian direction.
--- @return vector_old_c
function angle_old_c:to_backward_vector()
	local degrees_to_radians = function(degrees) return degrees * math.pi / 180 end

	local sp = math.sin(degrees_to_radians(self.p))
	local cp = math.cos(degrees_to_radians(self.p))
	local sy = math.sin(degrees_to_radians(self.y))
	local cy = math.cos(degrees_to_radians(self.y))

	return -vector(cp * cy, cp * sy, -sp)
end

--- Return a left vector of the angle. Use this to convert an angle into a cartesian direction.
--- @return vector_old_c
function angle_old_c:to_left_vector()
	local degrees_to_radians = function(degrees) return degrees * math.pi / 180 end

	local sp = math.sin(degrees_to_radians(self.p))
	local cp = math.cos(degrees_to_radians(self.p))
	local sy = math.sin(degrees_to_radians(self.y))
	local cy = math.cos(degrees_to_radians(self.y))
	local sr = math.sin(degrees_to_radians(self.r))
	local cr = math.cos(degrees_to_radians(self.r))

	return -vector(sr * sp * cy * -1 + cr * sy, sr * sp * sy * -1 + -1 * cr * cy, -1 * sr * cp)
end

--- Return a down vector of the angle. Use this to convert an angle into a cartesian direction.
--- @return vector_old_c
function angle_old_c:to_down_vector()
	local degrees_to_radians = function(degrees) return degrees * math.pi / 180 end

	local sp = math.sin(degrees_to_radians(self.p))
	local cp = math.cos(degrees_to_radians(self.p))
	local sy = math.sin(degrees_to_radians(self.y))
	local cy = math.cos(degrees_to_radians(self.y))
	local sr = math.sin(degrees_to_radians(self.r))
	local cr = math.cos(degrees_to_radians(self.r))

	return -vector(cr * sp * cy + sr * sy, cr * sp * sy + sr * cy * -1, cr * cp)
end

--- Calculate where a vector is in a given field of view.
--- @param source_vector vector_old_c
--- @param target_vector vector_old_c
--- @return number
function angle_old_c:fov_to(source_vector, target_vector)
	local fwd = self:to_forward_vector();
	local delta = (target_vector - source_vector):normalized();
	local fov = math.acos(fwd:dot_product(delta) / delta:length());

	return math.max(0.0, math.deg(fov));
end
--endregion
--endregion

--region dependency: havoc_menu_1_3_0
--region menu_assert
--- Assert.
--- @param expression boolean
--- @param level number
--- @param message string
--- @vararg string
--- @return void
local function menu_assert(expression, level, message, ...)
	if (not expression) then
		error(string.format(message, ...), level)
	end
end
--endregion

--region menu_map
local menu_map = {
	rage = {"aimbot", "other"},
	aa = {"anti-aimbot angles", "fake lag", "other"},
	legit = {"weapon type", "aimbot", "triggerbot", "other"},
	visuals = {"player esp", "other esp", "colored models", "effects"},
	misc = {"miscellaneous", "settings", "lua", "other"},
	skins = {"weapon skin", "knife options", "glove options"},
	config = {"presets", "lua"},
	players = {"players", "adjustments"},
	lua = {"a", "b"}
}

for tab, containers in pairs(menu_map) do
	menu_map[tab] = {}

	for i=1, #containers do
		menu_map[tab][containers[i]] = true
	end
end
--endregion

--region menu_item
--- @class menu_item_c
--- @field public tab string
--- @field public container string
--- @field public name string
--- @field public reference number
--- @field public visible boolean
--- @field public hidden_value any
--- @field public children table<number, menu_item_c>
--- @field public ui_callback function
--- @field public getter table
--- @field public setter table
--- @field public parent_value_or_callback any|function
local menu_item_c = {}

local menu_item_mt = {
	__index = menu_item_c
}

--- @param item menu_item_c
--- @vararg any
--- @return menu_item_c|any
function menu_item_mt.__call(item, ...)
	local args = {...}

	if (#args == 0) then
		return item:get()
	end

	local do_ui_set = {pcall(item.set, item, unpack(args))}

	menu_assert(do_ui_set[1], 4, do_ui_set[2])

	return item
end

--- Create a new menu_item_c.
--- @param element function
--- @param tab string
--- @param container string
--- @param name string
--- @vararg any
--- @return menu_item_c
function menu_item_c.new(element, tab, container, name, ...)
	local reference
	local is_menu_reference = false

	if ((type(element)) == "function") then
		local do_ui_new = { pcall(element, tab, container, name, ...)}

		menu_assert(do_ui_new[1], 4, "Cannot create menu item because: %s", do_ui_new[2])

		reference = do_ui_new[2]
	else
		reference = element
		is_menu_reference = true
	end

	return setmetatable(
		{
			tab = tab,
			container = container,
			name = name,
			reference = reference,
			visible = true,
			hidden_value = nil,
			children = {},
			ui_callback = nil,
			callbacks = {},
			is_menu_reference = is_menu_reference,
			getter = {
				callback = nil,
				data = nil
			},
			setter = {
				callback = nil,
				data = nil
			},
			parent_value_or_callback = nil
		},
		menu_item_mt
	)
end

--- @param value any
--- @return void
function menu_item_c:set_hidden_value(value)
	self.hidden_value = value
end

--- @vararg any
--- @return void
function menu_item_c:set(...)
	local args = {...}

	if (self.setter.callback ~= nil) then
		args = self.setter.callback(unpack(args))
	end

	local do_ui_set = {pcall(ui.set, self.reference, unpack(args))}

	menu_assert(do_ui_set[1], 3, "Cannot set values of menu item because: %s", do_ui_set[2])
end

--- @return any
function menu_item_c:get()
	if (self.visible == false and self.hidden_value ~= nil) then
		return self.hidden_value
	end

	local get = {ui.get(self.reference)}

	if (self.getter.callback ~= nil) then
		return self.getter.callback(get)
	end

	return unpack(get)
end

--- @param should_call boolean
--- @param var any
--- @return void
function menu_item_c:cache(should_call, var)
	if package._gcache == nil then
		package._gcache = { }
	end

	local name, _cond = 
		self.name, 
		ui.get(self.reference)

	local _type = type(_cond)
	local _, mode = ui.get(self.reference)
	local finder = mode or (_type == 'boolean' and tostring(_cond) or _cond)

	package._gcache[name] = package._gcache[name] or finder

	local hotkey_modes = { [0] = 'always on', [1] = 'on hotkey', [2] = 'toggle', [3] = 'off hotkey' }

	if should_call then ui.set(self.reference, mode ~= nil and hotkey_modes[var] or var) else
		if package._gcache[name] ~= nil then
			local _cache = package._gcache[name]

			if _type == 'boolean' then
				if _cache == 'true' then _cache = true end
				if _cache == 'false' then _cache = false end
			end

			ui.set(self.reference, mode ~= nil and hotkey_modes[_cache] or _cache)
			package._gcache[name] = nil
		end
	end
end

--- @param callback function
--- @param data any
--- @return void
function menu_item_c:set_setter_callback(callback, data)
	menu_assert(type(callback) == "function", 3, "Cannot set menu item setter callback: argument must be a function.")

	self.setter.callback = callback
	self.setter.data = data
end

--- @param callback function
--- @param data any
--- @return void
function menu_item_c:set_getter_callback(callback, data)
	menu_assert(type(callback) == "function", 3, "Cannot set menu item getter callback: argument must be a function.")

	self.getter.callback = callback
	self.getter.data = data
end

--- @param children table<any, menu_item_c>
--- @param value_or_callback function|any
--- @return void
function menu_item_c:add_children(children, value_or_callback)
	if (value_or_callback == nil) then
		value_or_callback = true
	end

	if (getmetatable(children) == menu_item_mt) then
		children = {children}
	end

	for _, child in pairs(children) do
		menu_assert(getmetatable(child) == menu_item_mt, 3, "Cannot add child to menu item: children must be menu item objects. Make sure you are not trying to parent a UI reference.")
		menu_assert(child.reference ~= self.reference, 3, "Cannot parent a menu item to iself.")

		child.parent_value_or_callback = value_or_callback
		self.children[child.reference] = child
	end

	menu_item_c._process_callbacks(self)
end

--- @param callback function
--- @return void
function menu_item_c:add_callback(callback)
	menu_assert(self.is_menu_reference == false, 3, "Cannot add callbacks to built-in menu items.")
	menu_assert(type(callback) == "function", 3, "Callbacks for menu items must be functions.")

	table.insert(self.callbacks, callback)

	menu_item_c._process_callbacks(self)
end

--- @vararg boolean
--- @return void
function menu_item_c:set_visible(value)

	local do_ui_set_visible = { pcall(ui.set_visible, self.reference, value) }

	self.visible = value

	menu_assert(do_ui_set_visible[1], 3, "Cannot set visible of menu item because: %s", do_ui_set_visible[2])
end

--- @param item menu_item_c
--- @return void
function menu_item_c._process_callbacks(item)
	local callback = function()
		for _, child in pairs(item.children) do
			local is_child_visible

			if (type(child.parent_value_or_callback) == "function") then
				is_child_visible = child.parent_value_or_callback()
			else
				is_child_visible = item:get() == child.parent_value_or_callback
			end

			local is_visible = (is_child_visible == true) and (item.visible == true)
			child.visible = is_visible

			ui.set_visible(child.reference, is_visible)

			if (child.ui_callback ~= nil) then
				child.ui_callback()
			end
		end

		for i = 1, #item.callbacks do
			item.callbacks[i]()
		end
	end

	ui.set_callback(item.reference, callback)
	item.ui_callback = callback

	callback()
end
--endregion

--region menu_manager
--- @class menu_manager_c
--- @field public tab string
--- @field public container string
--- @field public children table<number, menu_item_c>
local menu_manager_c = {}

local menu_manager_mt = {
	__index = menu_manager_c
}

--- Create a new menu_manager_c.
--- @param tab string
--- @param container string
--- @return menu_manager_c
function menu_manager_c.new(tab, container)
	menu_manager_c._validate_tab_container(tab, container)

	return setmetatable(
		{
			tab = tab,
			container = container,
			children = {}
		},
		menu_manager_mt
	)
end

--- Saves the values for menu items currently created to the database.
--- @return void
function menu_manager_c:save_to_db()
	local prefix = string.format("%s_%s", self.tab, self.container)

	for _, item in pairs(self.children) do
		local key = string.format("%s_%s", prefix, item.name)
		local data = {item()}

		database.write(key, data)
	end
end

--- Loads the values for menu items currently created from to the database.
--- @return void
function menu_manager_c:load_from_db()
	local prefix = string.format("%s_%s", self.tab, self.container)

	for _, item in pairs(self.children) do
		local key = string.format("%s_%s", prefix, item.name)
		local data = database.read(key)

		if (data ~= nil) then
			item(unpack(data))
		end
	end
end

--- @param item menu_item_c
--- @param value_or_callback function|any
--- @return void
function menu_manager_c:parent_all_to(item, value_or_callback)
	local children = self.children

	children[item.reference] = nil

	item:add_children(children, value_or_callback)
end

--- @param tab string
--- @param container string
--- @param name string
--- @return menu_item_c
function menu_manager_c.reference(tab, container, name)
	menu_manager_c._validate_tab_container(tab, container)

	local do_reference = {pcall(ui.reference, tab, container, name)}

	menu_assert(do_reference[1], 3, "Cannot reference Gamesense menu item because: %s", do_reference[2])

	local references = {select(2, unpack(do_reference))}
	local items = {}

	for i = 1, #references do
		table.insert(
			items,
			menu_item_c.new(
				references[i],
				tab,
				container,
				name
			)
		)
	end

	return unpack(items)
end

--- @param name string
--- @return menu_item_c
function menu_manager_c:checkbox(name)
	return self:_create_item(ui.new_checkbox, name)
end

--- @param name string
--- @param min number
--- @param max number
--- @param default_or_options number|table<any, any>
--- @return menu_item_c
function menu_manager_c:slider(name, min, max, default_or_options, show_tooltip, unit, scale, tooltips)
	if (type(default_or_options) == "table") then
		local options = default_or_options

		default_or_options = options.default
		show_tooltip = options.show_tooltip
		unit = options.unit
		scale = options.scale
		tooltips = options.tooltips
	end

	default_or_options = default_or_options or nil
	show_tooltip = show_tooltip or true
	unit = unit or nil
	scale = scale or 1
	tooltips = tooltips or nil

	menu_assert(type(min) == "number", 3, "Slider min value must be a number.")
	menu_assert(type(max) == "number", 3, "Slider max value must be a number.")
	menu_assert(min < max, 3, "Slider min value must be below the max value.")

	if (default_or_options ~= nil) then
		menu_assert(default_or_options >= min and default_or_options <= max, 3, "Slider default must be between min and max values.")
	end

	return self:_create_item(ui.new_slider, name, min, max, default_or_options, show_tooltip, unit, scale, tooltips)
end

--- @param name string
--- @vararg string
--- @return menu_item_c
function menu_manager_c:combobox(name, ...)
	local args = {...}

	if (type(args[1]) == "table") then
		args = args[1]
	end

	return self:_create_item(ui.new_combobox, name, args)
end

--- @param name string
--- @vararg string
--- @return menu_item_c
function menu_manager_c:multiselect(name, ...)
	local args = {...}

	if (type(args[1]) == "table") then
		args = args[1]
	end

	return self:_create_item(ui.new_multiselect, name, args)
end

--- @param name string
--- @param inline boolean
--- @return menu_item_c
function menu_manager_c:hotkey(name, inline)
	if (inline == nil) then
		inline = false
	end

	menu_assert(type(inline) == "boolean", 3, "Hotkey inline argument must be a boolean.")

	return self:_create_item(ui.new_hotkey, name, inline)
end

--- @param name string
--- @param callback function
--- @return menu_item_c
function menu_manager_c:button(name, callback)
	menu_assert(type(callback) == "function", 3, "Cannot set button callback because the callback argument must be a function.")

	return self:_create_item(ui.new_button, name, callback)
end

--- @param name string
--- @param r number
--- @param g number
--- @param b number
--- @param a number
--- @return menu_item_c
function menu_manager_c:color_picker(name, r, g, b, a)
	r = r or 255
	g = g or 255
	b = b or 255
	a = a or 255

	menu_assert(type(r) == "number" and r >= 0 and r <= 255, 3, "Cannot set color picker red channel value. It must be between 0 and 255.")
	menu_assert(type(g) == "number" and g >= 0 and g <= 255, 3, "Cannot set color picker green channel value. It must be between 0 and 255.")
	menu_assert(type(b) == "number" and b >= 0 and b <= 255, 3, "Cannot set color picker blue channel value. It must be between 0 and 255.")
	menu_assert(type(a) == "number" and a >= 0 and a <= 255, 3, "Cannot set color picker alpha channel value. It must be between 0 and 255.")

	return self:_create_item(ui.new_color_picker, name, r, g, b, a)
end

--- @param name string
--- @return menu_item_c
function menu_manager_c:textbox(name)
	return self:_create_item(ui.new_textbox, name)
end

--- @param name string
--- @vararg string
--- @return menu_item_c
function menu_manager_c:listbox(name, ...)
	local args = {...}

	if (type(args[1]) == "table") then
		args = args[1]
	end

	local item = self:_create_item(ui.new_listbox, name, args)

	item:set_getter_callback(
		function(get)
			return item.getter.data[get + 1]
		end,
		args
	)

	return item
end

--- @param name string
--- @return menu_item_c
function menu_manager_c:label(name)
	menu_assert(type(name) == "string", "Label name must be a string.")

	return self:_create_item(ui.new_label, name)
end

--- @param element function
--- @param name string
--- @vararg any
--- @return menu_item_c
function menu_manager_c:_create_item(element, name, ...)
	menu_assert(type(name) == "string" and name ~= "", 3, "Cannot create menu item: name must be a non-empty string.")

	local item = menu_item_c.new(element, self.tab, self.container, name, ...)
	self.children[item.reference] = item

	return item
end

--- @param tab string
--- @param container string
--- @return void
function menu_manager_c._validate_tab_container(tab, container)
	menu_assert(type(tab) == "string" and tab ~= "", 4, "Cannot create menu manager: tab name must be a non-empty string.")
	menu_assert(type(container) == "string" and container ~= "", 4, "Cannot create menu manager: tab name must be a non-empty string.")

	tab = tab:lower()

	menu_assert(menu_map[tab] ~= nil, 4, "Cannot create menu manager: tab name does not exist.")
	menu_assert(menu_map[tab][container:lower()] ~= nil, 4, "Cannot create menu manager: container name does not exist.")
end
--endregion
--endregion

local dt_fakelag = menu_manager_c.reference( 'RAGE', 'Other', 'Double tap fake lag limit' )
local usrcmd_maxpticks = menu_manager_c.reference( 'MISC', 'Settings', 'sv_maxusrcmdprocessticks' )
local hold_aim = menu_manager_c.reference( 'MISC', 'Settings', 'sv_maxusrcmdprocessticks_holdaim' )
local clockcorretion_msecs = menu_manager_c.reference( 'MISC', 'Settings', 'sv_clockcorrection_msecs' )

local fakelag = menu_manager_c.reference( 'AA', 'Fake lag', 'Limit' )
local doubletap, doubletap_hk = menu_manager_c.reference( 'RAGE', 'Other', 'Double tap' )
local double_tap_mode = menu_manager_c.reference( 'RAGE', 'Other', 'Double tap mode' )
local onshotaa, onshotaa_hk = menu_manager_c.reference( 'AA', 'Other', 'On shot anti-aim' )

local menu = menu_manager_c.new("AA", "Other")
local active_script = menu:checkbox("[4elikcord] Double tap handler")
local shift_ticks = menu:slider(" > Shift ticks", 15, 21, 16)
local shot_boost = menu:checkbox(" > Double tap shot handler")
local animation_handler = menu:checkbox(" > Double tap animation handler")

active_script:add_children({
	shift_ticks,
	shot_boost,
	animation_handler
})

local count = function(tab)
	local count = 0

	for _, _ in pairs(tab) do
		count = count + 1
	end

	return count
end

--region tickbase_controller

local createmove_c = {}
local createmove_mt = { __index = createmove_c }

local aimbot = {}
local aimbot_mt = { __index = aimbot }

function aimbot.new()
    return setmetatable({
		data = { },
		shift_time = 0,
		shift_data = { },
		misses = 0,
		reason = 'spread',
		ticks = 0
    }, aimbot_mt)
end

function aimbot:fire(e, createmove_c)
    local run_qm = false
    local is_inactive = self.shift_time == 0

    --print(self.shift_time)

    if double_tap_mode:get() == 'Offensive' and is_inactive and createmove_c.can_shift_tickbase == 2 then
        self.shift_time = 1
        self.data[count(self.data)+1] = { e.x, e.y, e.z }

        run_qm = true
    end
end

local ffi = require("ffi")

ffi.cdef[[
	typedef struct
	{
		char pad_vtable[ 0x4 ];         // 0x0
		char* consoleName;              // 0x4
		char pad_0[ 0xc ];              // 0x8
		int iMaxClip1;                  // 0x14
		int iMaxClip2;                  // 0x18
		int iDefaultClip1;              // 0x1c
		int iDefaultClip2;              // 0x20
		int iPrimaryReserveAmmoMax;     // 0x24
		int iSecondaryReserveAmmoMax;   // 0x28
		char* szWorldModel;             // 0x2c
		char* szViewModel;              // 0x30
		char* szDroppedModel;           // 0x34
		char pad_9[ 0x50 ];             // 0x38
		char* szHudName;                // 0x88
		char* szWeaponName;             // 0x8c
		char pad_11[ 0x2 ];             // 0x90
		bool bIsMeleeWeapon;            // 0x92
		char pad_12[ 0x9 ];             // 0x93
		float flWeaponWeight;           // 0x9c
		char pad_13[ 0x2c ];            // 0xa0
		int iWeaponType;                // 0xcc
		int iWeaponPrice;               // 0xd0
		int iKillAward;                 // 0xd4
		char pad_16[ 0x4 ];             // 0xd8
		float flCycleTime;              // 0xdc
		float flCycleTimeAlt;           // 0xe0
		char pad_18[ 0x8 ];             // 0xe4
		bool bFullAuto;                 // 0xec
		char pad_19[ 0x3 ];             // 0xed
		int iDamage;                    // 0xf0
		float flArmorRatio;             // 0xf4
		int iBullets;                   // 0xf8
		float flPenetration;            // 0xfc
		char pad_23[ 0x8 ];             // 0x100
		float flWeaponRange;            // 0x108
		float flRangeModifier;          // 0x10c
		float flThrowVelocity;          // 0x110
		char pad_26[ 0xc ];             // 0x114
		bool bHasSilencer;              // 0x120
		char pad_27[ 0xb ];             // 0x121
		char* szBulletType;             // 0x12c
		float flMaxSpeed;               // 0x130
		char pad_29[ 0x50 ];            // 0x134
		int iRecoilSeed;                // 0x184
	} lib_gamesense_entityWeaponData_t;
	typedef lib_gamesense_entityWeaponData_t* (__thiscall *lib_gamesense_struct_entityWeaponData)(void*);

	typedef void*(__thiscall* get_client_entity_t)(void*, int); // 3

	struct lib_gamesense_animlayer_t {	
        char pad20[24];	
        uint32_t m_nSequence;	
        float m_flPrevCycle;	
        float m_flWeight;	
        char pad20[8];	
        float m_flCycle;	
        void *m_pOwner;	
        char pad_0038[ 4 ];	
    };	

    struct lib_gamesense_animstate_t { 	
        char pad[ 3 ];	
        char m_bForceWeaponUpdate; //0x4	
        char pad1[ 91 ];	
        void* m_pBaseEntity; //0x60	
        void* m_pActiveWeapon; //0x64	
        void* m_pLastActiveWeapon; //0x68	
        float m_flLastClientSideAnimationUpdateTime; //0x6C	
        int m_iLastClientSideAnimationUpdateFramecount; //0x70	
        float m_flAnimUpdateDelta; //0x74	
        float m_flEyeYaw; //0x78	
        float m_flPitch; //0x7C	
        float m_flGoalFeetYaw; //0x80	
        float m_flCurrentFeetYaw; //0x84	
        float m_flCurrentTorsoYaw; //0x88	
        float m_flUnknownVelocityLean; //0x8C	
        float m_flLeanAmount; //0x90	
        char pad2[ 4 ];	
        float m_flFeetCycle; //0x98	
        float m_flFeetYawRate; //0x9C	
        char pad3[ 4 ];	
        float m_fDuckAmount; //0xA4	
        float m_fLandingDuckAdditiveSomething; //0xA8	
        char pad4[ 4 ];	
        float m_vOriginX; //0xB0	
        float m_vOriginY; //0xB4	
        float m_vOriginZ; //0xB8	
        float m_vLastOriginX; //0xBC	
        float m_vLastOriginY; //0xC0	
        float m_vLastOriginZ; //0xC4	
        float m_vVelocityX; //0xC8	
        float m_vVelocityY; //0xCC	
        char pad5[ 4 ];	
        float m_flUnknownFloat1; //0xD4	
        char pad6[ 8 ];	
        float m_flUnknownFloat2; //0xE0	
        float m_flUnknownFloat3; //0xE4	
        float m_flUnknown; //0xE8	
        float m_flSpeed2D; //0xEC	
        float m_flUpVelocity; //0xF0	
        float m_flSpeedNormalized; //0xF4	
        float m_flFeetSpeedForwardsOrSideWays; //0xF8	
        float m_flFeetSpeedUnknownForwardOrSideways; //0xFC	
        float m_flTimeSinceStartedMoving; //0x100	
        float m_flTimeSinceStoppedMoving; //0x104	
        bool m_bOnGround; //0x108	
        bool m_bInHitGroundAnimation; //0x109	
        float m_flTimeSinceInAir; //0x10A	
        float m_flLastOriginZ; //0x10E	
        float m_flHeadHeightOrOffsetFromHittingGroundAnimation; //0x112	
        float m_flStopToFullRunningFraction; //0x116	
        char pad7[ 4 ]; //0x11A	
        float m_flMagicFraction; //0x11E	
        char pad8[ 60 ]; //0x122	
        float m_flWorldForce; //0x15E	
        char pad9[ 462 ]; //0x162	
        float m_flMaxYaw; //0x334	
    };
]]

local classptr = ffi.typeof('void***')
local rawientitylist = client.create_interface('client_panorama.dll', 'VClientEntityList003') or error('VClientEntityList003 wasnt found', 2)
local ientitylist = ffi.cast(classptr, rawientitylist) or error('rawientitylist is nil', 2)
local get_client_entity = ffi.cast('get_client_entity_t', ientitylist[0][3]) or error('get_client_entity is nil', 2)
local get_client_networkable = ffi.cast('void*(__thiscall*)(void*, int)', ientitylist[0][0]) or error('get_client_networkable_t is nil', 2)
local rawivmodelinfo = client.create_interface('engine.dll', 'VModelInfoClient004')	
local ivmodelinfo = ffi.cast(classptr, rawivmodelinfo) or error('rawivmodelinfo is nil', 2)	
local get_studio_model = ffi.cast('void*(__thiscall*)(void*, const void*)', ivmodelinfo[0][32])
local function get_model(b)if b then b=ffi.cast(classptr,b)local c=ffi.cast(crr_t,b[0][0])local d=c(b)or error('error getting client unknown',2)if d then d=ffi.cast(classptr,d)local e=ffi.cast(cr_t,d[0][5])(d)or error('error getting client renderable',2)if e then e=ffi.cast(classptr,e)return ffi.cast(gm_t,e[0][8])(e)or error('error getting model_t',2)end end end end	
local function get_sequence_activity(b,c,d)b=ffi.cast(classptr,b)local e=get_studio_model(ivmodelinfo,get_model(c))if e==nil then return-1 end;local f=ffi.cast(gsa_t, seq_activity_sig)return f(b,e,d)end	
local function get_anim_layer(b,c)c=c or 1;b=ffi.cast(classptr,b)return ffi.cast('struct lib_gamesense_animlayer_t**',ffi.cast('char*',b)+0x2980)[0][c]end


function createmove_c.new()
	return setmetatable({
        old_tickbase = 0,
        old_sim_time = 0,
        old_command_num = 0,
        skip_next_differ = false,
        charged_before = false,
    
        did_shift_before = false,
        can_shift_tickbase = 0,
        is_cmd_safe = true,
        last_charge = 0,
        
        charge = false,
        validate_cmd = 17,
        
        lag_state = nil,
        delay = 0,
        doubletapped = false,
        should_break_tbc = false,
    }, createmove_mt)
end

function createmove_c:process(e)
    local next_shift_amount = 0	
    self.should_break_tbc = false	

    local me = entity.get_local_player()	
    local wpn = entity.get_player_weapon(me)	

    local wpn_name = entity.get_classname(wpn) or ''	
    local wpn_id = entity.get_prop(wpn, 'm_iItemDefinitionIndex')	
    local m_item = wpn_id and bit.band(wpn_id, 0xFFFF) or 0		

    local nextAttack = entity.get_prop(me, "m_flNextAttack") 
    local nextShot = entity.get_prop(wpn, "m_flNextPrimaryAttack")
    local nextShotSecondary = entity.get_prop(wpn, "m_flNextSecondaryAttack")

    local can_exploit = function(me, wpn, ticks_to_shift)	
        if wpn == nil then	
            return false	
        end	

        local tickbase = entity.get_prop(me, 'm_nTickBase')	
        local curtime = globals.tickinterval() * (tickbase-ticks_to_shift)	

        if curtime < entity.get_prop(me, 'm_flNextAttack') then	
            return false	
        end	

        if curtime < entity.get_prop(wpn, 'm_flNextPrimaryAttack') then	
            return false	
        end	

        return true	
    end	

	if self.validate_cmd > 0 then		
		self.validate_cmd = self.validate_cmd-1		

		local dt, dt_key = doubletap:get(), doubletap_hk:get()

		if dt and dt_key then		
			self.should_break_tbc = true		
		end		
	end	

	::begin_command::

    local ready_to_shift = can_exploit(me, wpn, 16)	
    local weapon_ready = can_exploit(me, wpn, math.abs(-1 - next_shift_amount))	

    if (ready_to_shift == true or weapon_ready == false) and self.did_shift_before == true then	
        next_shift_amount = 16
    else	
        next_shift_amount = 0	
    end	

    local tickbase = entity.get_prop(me, 'm_nTickBase')	

	local difference = e.command_number - self.old_command_num	
	
    if nextShot and nextShotSecondary and math.max(nextShot,nextShotSecondary) < nextAttack and nextAttack - globals.curtime() > 0.00 then
        self.skip_next_differ = true	
        self.charged_before = true	
        self.can_shift_tickbase = false
    end

    if difference >= 11 and difference <= 17 then
        self.can_shift_tickbase = true	
        self.charged_before = true	
        self.last_charge = difference+1	

        self.is_cmd_safe = difference > 3 and math.abs(17-difference) <= 3		
    end	

    if globals.chokedcommands() > 1 and self.charged_before then 
        self.charged_before = false	
        self.can_shift_tickbase = false
    end

    if ready_to_shift == false then	
        self.can_shift_tickbase = false
    end	

    self.old_tickbase = tickbase	
    self.old_command_num = e.command_number	

    self.skip_next_differ = false	
    self.did_shift_before = next_shift_amount ~= 0	

    self.can_shift_tickbase = ( self.can_shift_tickbase and self.last_charge > 0 ) and 2 or 0	

    if self.can_shift_tickbase == 0 and self.charged_before == true then	
        self.can_shift_tickbase = 1	
	end

    -- Reset tickbase shift data on doubletap reset	
    if self.can_shift_tickbase == 0 then	
        self.last_charge = 0	
    end
end

function createmove_c:handler(e, caimbot)

    local me = entity.get_local_player()
    local shots_fired = entity.get_prop(me, "m_iShotsFired")
    local m_vecvel = { entity.get_prop(me, 'm_vecVelocity') }
    local velocity = math.floor(math.sqrt(m_vecvel[1]^2 + m_vecvel[2]^2 + m_vecvel[3]^2) + 0.5)

    if self.lag_state ~= nil and active_script:get() and animation_handler:get()  then
        hold_aim:set(self.lag_state)
        self.lag_state = nil
    end
    
    --local osa, osa_key = onshot_aa:call()
    local dt, dt_key = doubletap:get(), doubletap_hk:get()

    local cs_tickbase = self.can_shift_tickbase

    ::begin_command::

    local losc = dt and dt_key and double_tap_mode:get() == 'Offensive'

    local fired_this_tick = false
    local reset_command = false

    self.can_break_lby = false-- and lowerbody:call() == "Opposite" and velocity <= 1.01

    if caimbot.shift_time > 0 and active_script:get() then
        local current_command = e

        local max_commands = self.last_charge
        local aimbot_command = caimbot.data[count(caimbot.data)]

        if count(caimbot.data) > 0 and aimbot_command ~= nil and shot_boost:get() then
            
            local eye_pos = vector(client.eye_position())
            local fire_vector = vector(unpack(aimbot_command))
    
            local entindex, dmg = eye_pos:trace_bullet_to(me, fire_vector)
            local ent_valid = entity.is_alive(entindex)
            local aim_at = eye_pos:angle_to(fire_vector)
            local health = entity.get_prop(entindex, "m_iHealth")

            if ent_valid and dmg < health and losc and cs_tickbase == 2 then
                --e.in_attack = 1
            end

            if caimbot.shift_time == max_commands or max_commands < 1 then
                e.pitch = aim_at.p
                e.yaw = aim_at.y
                
                if ( dmg >= 0 or ent_valid ) then
                    fired_this_tick = true
                    reset_command = true
                end
			end
			e.in_attack = 1
        end

        caimbot.shift_data[#caimbot.shift_data+1] = {
            caimbot.shift_time, cs_tickbase, e.chokedcommands, entity.get_prop(me, 'm_nTickBase'), globals.tickcount(), 'false'
        }

        if caimbot.shift_time ~= 0 and (reset_command == true or caimbot.shift_time == max_commands or max_commands < 1) then

            caimbot.shift_time = 0
            caimbot.shift_data = { }
            caimbot.data = { }

            current_command = e
        else
            caimbot.shift_time = caimbot.shift_time + 1
        end
    end

    if caimbot.shift_time == 0 and fired_this_tick == false and (	
        (losc == true and cs_tickbase == 0) or 	
        (velocity <= 1 and cs_tickbase == 2)	
    ) then	
        self.lag_state = dt	

        if count(caimbot.shift_data) > 0 then	
            caimbot.shift_data[count(caimbot.shift_data)][6] = tostring(dt)	
        end	
    end

    --double_tap:set_cache(1, fired_this_tick or (losc == true and cs_tickbase == 1) and velocity > 1.01, false)

    if fired_this_tick or (losc == true and cs_tickbase == 1) and velocity > 1.01 then
        --print('work')
        
        --e.allow_send_packet = e.chokedcommands >= 3
	end

    --hold_aim:cache( fired_this_tick and active_script:get() and animation_handler:get() , false )
	usrcmd_maxpticks:cache(active_script:get(), 16 + (shift_ticks:get() - 15))
	--clockcorretion_msecs:cache(active_script:get(), 0)

    if self.lag_state ~= nil and active_script:get() and animation_handler:get()  then
        hold_aim:set(false)
    end
    self.doubletapped = cs_tickbase == 2
end

function createmove_c:run_command(e)
	if self.lag_state ~= nil and active_script:get() and animation_handler:get() then		
		hold_aim:set(self.lag_state)		
		self.lag_state = nil		
	end
end

local caimbot = aimbot.new()
local createmove = createmove_c.new()

client.set_event_callback("aim_fire", function(e)
    caimbot:fire(e, createmove)
end)

client.set_event_callback("predict_command", function(e)
    createmove:process(e)
end)

client.set_event_callback("setup_command", function(e)
    createmove:handler(e, caimbot)
end)

client.set_event_callback("run_command", function(e)
	createmove:run_command(e)
end)
