defmodule BuildPipeline.TerminalWidth.TputCols do
  def run do
    System.cmd("tput", ["cols"])
  end
end
