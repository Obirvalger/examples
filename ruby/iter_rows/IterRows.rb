class IterRows
  attr_reader :size
  attr_reader :rows

  def initialize(size)
    @size = size
    @rows = make_rows(size)
  end

  def make_rows(n)
    rows = [init_row]

    1.upto(n) do |i|
      rows.push(iter(rows[i-1]))
    end

    return rows
  end

  def to_str
    res = ''
    len = row_to_s(rows[-1]).length
    rows.each do |r|
      str = row_to_s(r).center(len, fill_space)
      res += str + "\n"
    end
    return res
  end

  alias inspect to_str
  alias to_s to_str

  protected 

  def fill_space
    ' '
  end

  def row_to_s(r)
    r.map(&:to_s).join
  end

  def init_row()
    raise NotImplementedError
  end

  def iter(row)
    raise NotImplementedError
  end
end
