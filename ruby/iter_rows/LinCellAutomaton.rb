load 'IterRows.rb'

class LCA < IterRows
  attr_reader :function

  def default_size
    13
  end

  def post_initialize(args)
    #func = args[:function] || "01011010"
    func = args[:function] || rand(256).to_s(2).rjust(8,'0')
    puts "Function #{func}"
    @function = func.split('').map(&:to_i)
  end

  def init_row 
    [0,0,1,0,0]
  end

  def iter(row)
    new_row = []
    #puts "Lol #{row}"
    1.upto(row.size - 2) do |j|
      #puts "j = #{j}"
      #new_row[j-1] = (0 ^ row[j-1] ^ row[j+1]) ^ (row[j] | 1)
      new_row[j-1] = function[4*row[j-1] + 2*row[j] + row[j+1]]
    end
    return [0,0] + new_row + [0,0]
  end

  def row_to_s(r)
    r.map(&:to_s).join.tr("01", " #")
  end

  def fill_space
    ' '
  end
end
