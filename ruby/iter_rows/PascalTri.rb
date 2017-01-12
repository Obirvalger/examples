load 'IterRows.rb'

class PascalTri < IterRows
  def default_size
    4
  end

  def init_row
    [0,1,0]
  end

  def iter(row)
    new_row = []
    0.upto(row.size - 2) do |j|
      new_row[j] = row[j] + row[j+1]
    end
    return [0] + new_row + [0]
  end

  def row_to_s(r)
    r[1..-2].join(' ')
  end
end

