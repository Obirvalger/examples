load 'IterRows.rb'

class LCA < IterRows
  def init_row 
    [0,0,1,0,0]
  end

  def iter(row)
    new_row = []
    #puts "Lol #{row}"
    1.upto(row.size - 2) do |j|
      #puts "j = #{j}"
      new_row[j-1] = (0 ^ row[j-1] ^ row[j+1]) ^ (row[j] | 1)
    end
    return [0,0] + new_row + [0,0]
  end

  def row_to_s(r)
    r.map(&:to_s).join.tr("01", " O")
  end

  def fill_space
    ' '
  end
end
