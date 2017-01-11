require 'matrix'

class Vector
  def dist(other)
    (self - other).to_a.map(&:abs).count(1)
  end
end

class Integer
  def to_vec(n)
    Vector[*sprintf("%0#{n}b", self).split('').map(&:to_i)]
  end
end

def all_vectors(n)
  (0..2**n-1).map {|i| i.to_vec(n)}
end

