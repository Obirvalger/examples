defmodule Math do
  def pow(b, e) when is_integer e do
    if e == 0 do
      result = 1
    else 
      half = pow(b, div(e, 2))
      if rem(e, 2) == 1 do
        result = half * half * b
      else
        result = half * half
      end
    end
  end
    
  def pow(b,e), do: :math.pow(b,e)
    
  def gcd(a, b) when a >= 0 and b >= 0 and is_integer a and is_integer b do
    cond do
      a == 0 -> b
      b == 0 -> a
      a == b -> a
      a < b -> gcd(a, rem(b, a))
      a > b -> gcd(rem(a, b), b)
    end
  end
  
  def lcm(a,b) when a >= 0 and b >= 0 and is_integer a and is_integer b do
    div(a*b, gcd(a,b)) 
  end
  
  def factorial(n) when n >= 0 do
    cond do
      n == 0 -> 1
      true -> n * factorial(n - 1)
    end
  end

  def map(_, []), do: []
  
  def map(f, [x | xs]) do
    [f.(x) | map(f, xs)]
  end
  
  def filter(_, []), do: []
  
  def filter(p, [x | xs]) do
    if p.(x) do
      [x | filter(p, xs)]
    else
      filter(p,xs)
    end
  end
end
