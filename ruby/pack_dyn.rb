#! /usr/bin/ruby
require 'matrix'

$verbose = true

class Vector
    def []=(i, x)
        @elements[i] = x
    end
end

class Matrix
    def []=(i, j, x)
        @rows[i][j] = x
    end
end
       
def example_matrix()
    Matrix[[0,1,0,1],[1,0,1,0],[0,1,1,0]]
end

def almost_full(n)
    Matrix.build(n,n) {1} - Matrix.identity(n)
end

def random_matrix(n)
    m_min = n/2
    m_max = 2**n
    m = m_min + rand(m_max - m_min)
    $verbose = false if m > 5
    res = []
    m.times do
        res << Array.new(n) {rand(2)}
    end
    
    Matrix[*res.uniq.find_all{|v| v.any? {|x| x > 0}}.transpose]
end

def get_matrix()
    n = ARGV[1].to_i || 5
    #$verbose = false
    $verbose = false if n > 5
    case ARGV[0]
    when "0" then Matrix.I(n)
    when "1" then almost_full(n)
    when "2" then random_matrix(n)
    else example_matrix
    end
end

def pack_dyn(a)
    m, n = a.row_size, a.column_size
    x = [[Vector[*[0]*n], Vector[*[0]*m], 0]]
    n_steps = 0
    n.times do |j|
        sj = a.column(j)
        x.each do |sol, cov, size|
            newcov = cov + sj
            if newcov.max <= 1
                newsol = sol.clone
                newsol[j] = 1
                puts "column(#{j}) + <#{sol.to_a}, #{cov.to_a}>" if $verbose
                puts "  => #{sj.to_a}+#{cov.to_a}=#{newcov.to_a} => " +
                    "#{newsol.to_a}" if $verbose
                x << [newsol, newcov, size + 1]
            end
            n_steps += 1
        end
    end
    
    puts if $verbose
    puts "Steps: #{n_steps}"
    opt = x.map{|y| y[0].reduce(:+)}.max
    x.find_all{|i| i[0].reduce(:+) == opt}.map {|y| y[0].to_a}
end

m = get_matrix

puts "Matrix #{m.row_size}x#{m.column_size}:"
m.to_a.each {|y| p y}
puts

x = pack_dyn(m)

puts "\nMaximum #{x[0].count(1)} sets. Solution:"
x.each {|y| p y}

