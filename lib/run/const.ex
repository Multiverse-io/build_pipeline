defmodule BuildPipeline.Run.Const do
  def successful, do: "successful"
  def failed, do: "failed"
  def not_started, do: "not started"
  def skipped, do: "skipped"

  # keep value of this env var aligned with what it says in the README.md and usage_instructions
  def save_result_env_var_name, do: "BUILD_PIPELINE_SAVE_RESULT"
  def from_failed_env_var_name, do: "BUILD_PIPELINE_FROM_FAILED"

  def all_results, do: [successful(), failed(), not_started(), skipped()]
  def skipable_results, do: [successful(), skipped()]
  def unskipable_results, do: [failed(), not_started()]
end
