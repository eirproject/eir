defmodule Something.Nif do
  @on_load { :init, 0 }

  def init() do
    :ok = :erlang.load_nif("./woohoo", 0)
    :ok
  end

  def woo() do

  end

  def woohoo(1) do
    0
  end

end

IO.inspect Something.Nif.woohoo(4)
IO.inspect Something.Nif.woohoo(5)
IO.inspect Something.Nif.woohoo(6)
IO.inspect Something.Nif.woohoo(7)
IO.inspect Something.Nif.woohoo(:wat)
