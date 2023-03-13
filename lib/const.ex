defmodule BuildPipeline.Const do
  def successful, do: "successful"
  def failed, do: "failed"
  def not_started, do: "not started"
  def skipped, do: "skipped"

  def all_results, do: [successful(), failed(), not_started(), skipped()]
  def skipable_results, do: [successful(), skipped()]
  def unskipable_results, do: [failed(), not_started()]
end