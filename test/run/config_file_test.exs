defmodule BuildPipeline.Run.ConfigFileTest do
  use ExUnit.Case, async: true
  alias BuildPipeline.Run.ConfigFile

  @simple_example_dir "./example_projects/simple_and_functioning"
  @simple_example_json File.read!("#{@simple_example_dir}/build_pipeline/config.json")
  @setup %{cwd: "."}

  describe "read" do
    test "returns ok with the file's contents if it's there and readable" do
      setup = %{cwd: @simple_example_dir}
      assert {:ok, {file_contents, ^setup}} = ConfigFile.read(setup)

      assert file_contents == @simple_example_json
    end

    test "returns error given a nonsense file" do
      non_file_location = "./definitely/not/a/real/place/on/this/computer"
      expected_error = "./definitely/not/a/real/place/on/this/computer/build_pipeline/config.json"

      assert {:error, {:config_file_not_found, expected_error}} ==
               ConfigFile.read(%{cwd: non_file_location})
    end
  end

  describe "parse_and_validate/1" do
    test "when valid, returns the build pipeline" do
      assert {:ok, %{build_pipeline: build_pipeline}} =
               ConfigFile.parse_and_validate({@simple_example_json, @setup})

      assert build_pipeline == [
               %{
                 build_step_name: "sayHello",
                 command: "echo 'hello'",
                 command_env_vars: [],
                 depends_on: MapSet.new([]),
                 command_type: :shell_command,
                 order: 0,
                 status: :incomplete
               }
             ]
    end

    test "when valid, but more complex, retuns the build pipeline" do
      json = """
      [
        {"buildStepName": "tiresNotSlashed", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": []},
        {"buildStepName": "enoughFuel", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "carWorks", "commandType": "shellCommand", "command": "echo 'car works'", "dependsOn": ["tiresNotSlashed", "enoughFuel"]},
        {"buildStepName": "driveToOffice", "commandType": "shellCommand", "command": "echo 'drive'", "dependsOn": ["carWorks"]},
        {"buildStepName": "approachHuman", "commandType": "shellCommand", "command": "echo 'walk over'", "dependsOn": ["driveToOffice"]},
        {"buildStepName": "sayHello", "commandType": "shellCommand", "command": "echo 'hello'", "dependsOn": ["approachHuman"]}
      ]
      """

      assert {:ok, %{build_pipeline: build_pipeline}} =
               ConfigFile.parse_and_validate({json, @setup})

      assert build_pipeline == [
               %{
                 build_step_name: "tiresNotSlashed",
                 command: "echo 'tires'",
                 command_env_vars: [],
                 depends_on: MapSet.new([]),
                 command_type: :shell_command,
                 order: 0,
                 status: :incomplete
               },
               %{
                 build_step_name: "enoughFuel",
                 command: "echo 'fuel'",
                 command_env_vars: [],
                 depends_on: MapSet.new([]),
                 command_type: :shell_command,
                 order: 1,
                 status: :incomplete
               },
               %{
                 build_step_name: "carWorks",
                 command: "echo 'car works'",
                 command_env_vars: [],
                 depends_on: MapSet.new(["tiresNotSlashed", "enoughFuel"]),
                 command_type: :shell_command,
                 order: 2,
                 status: :incomplete
               },
               %{
                 build_step_name: "driveToOffice",
                 command: "echo 'drive'",
                 command_env_vars: [],
                 depends_on: MapSet.new(["carWorks"]),
                 command_type: :shell_command,
                 order: 3,
                 status: :incomplete
               },
               %{
                 build_step_name: "approachHuman",
                 command: "echo 'walk over'",
                 command_env_vars: [],
                 depends_on: MapSet.new(["driveToOffice"]),
                 command_type: :shell_command,
                 order: 4,
                 status: :incomplete
               },
               %{
                 build_step_name: "sayHello",
                 command: "echo 'hello'",
                 command_env_vars: [],
                 depends_on: MapSet.new(["approachHuman"]),
                 command_type: :shell_command,
                 order: 5,
                 status: :incomplete
               }
             ]
    end

    test "understands envVars in the json" do
      json = """
      [
        {"buildStepName": "compile", "commandType": "shellCommand", "command": "mix compile", "dependsOn": [], "envVars": [{"name": "MIX_ENV", "value": "test"}]}
      ]
      """

      assert {:ok, %{build_pipeline: build_pipeline}} =
               ConfigFile.parse_and_validate({json, @setup})

      assert build_pipeline == [
               %{
                 build_step_name: "compile",
                 command: "mix compile",
                 command_env_vars: [{'MIX_ENV', 'test'}],
                 depends_on: MapSet.new([]),
                 command_type: :shell_command,
                 order: 0,
                 status: :incomplete
               }
             ]
    end

    test "returns error given non-list nonsense env vars" do
      json = """
      [
        {"buildStepName": "compile", "commandType": "shellCommand", "command": "mix compile", "dependsOn": [], "envVars": "bollocks"}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a built step had bad envVars of \"bollocks\". They should be in the form \"envVars\": [{\"name\": \"MIX_ENV\", \"value\": \"test\"}]"}} ==
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "returns error given list nonsense env vars" do
      json = """
      [
        {"buildStepName": "compile", "commandType": "shellCommand", "command": "mix compile", "dependsOn": [], "envVars": [{"bollocks": "assy"}]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a built step had bad envVars of %{\"bollocks\" => \"assy\"}. They should be in the form \"envVars\": [{\"name\": \"MIX_ENV\", \"value\": \"test\"}]"}} ==
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "when a key is missing, returns error" do
      missing_key =
        "[\n  {\"commandType\": \"shellCommand\", \"command\": \"echo 'hello'\", \"dependsOn\": []}\n]\n"

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a build step was missing the key 'buildStepName'"}} ==
               ConfigFile.parse_and_validate({missing_key, @setup})
    end

    test "when a command_type is not valid, returns error" do
      missing_key =
        "[\n  {\"buildStepName\": \"sayHello\", \"commandType\": \"NONSENSE\", \"command\": \"echo 'hello'\", \"dependsOn\": []}\n]\n"

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a build step had an invalid commandType of 'NONSENSE'"}} ==
               ConfigFile.parse_and_validate({missing_key, @setup})
    end

    test "when a dependsOn is missing, returns error" do
      missing_key = """
          [
            {"buildStepName": "sayHello",
            "commandType": "shellCommand",
            "command": "echo 'hello'",
            "dependsOn": ["approachHuman"]
            }
          ]

      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a build step had a dependsOn of 'approachHuman' that does not exist"}} ==
               ConfigFile.parse_and_validate({missing_key, @setup})
    end

    test "dependsOn must be a list" do
      missing_key =
        "[\n  {\"buildStepName\": \"sayHello\", \"commandType\": \"shellCommand\", \"command\": \"echo 'hello'\", \"dependsOn\": \"ass\"}\n]\n"

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a build step had a non-list dependsOn of 'ass'"}} ==
               ConfigFile.parse_and_validate({missing_key, @setup})
    end

    test "fails when build_step_names are not unqiue" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": []},
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because a the buildStepName \"A\" was duplicated, but buildStepNames must be unique"}} ==
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "supports the 'script' comandType" do
      json = """
      [
        {"buildStepName": "A", "commandType": "script", "command": "echo_hello", "dependsOn": []}
      ]
      """

      assert {:ok, %{build_pipeline: build_pipeline}} =
               ConfigFile.parse_and_validate({json, @setup})

      assert [%{command: "echo_hello", command_type: :script}] = build_pipeline
    end

    test "returns error given invalid json" do
      json = "invalid nonsense"

      assert {:error, {:invalid_config, %Jason.DecodeError{}}} =
               ConfigFile.parse_and_validate({json, @setup})
    end
  end

  describe "no circular dependsOn references" do
    test "simplest failure case" do
      json = """
      [
        {"buildStepName": "tiresNotSlashed", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["enoughFuel"]},
        {"buildStepName": "enoughFuel", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["tiresNotSlashed"]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: enoughFuel <-> enoughFuel"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "depending on yourself" do
      json = """
      [
        {
        "buildStepName": "A",
        "commandType": "shellCommand",
        "command": "echo 'tires'",
        "dependsOn": ["A"]
      }
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "more nested case" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["C"]},
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["D"]},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["A"]},
        {"buildStepName": "D", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["B"]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "one depenecy is fine, but the other is bad" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["C", "B"]},
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["A"]},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "two depeneces are fine, but the other is bad" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["D", "C", "B"]},
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["A"]},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "D", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "its only found to be circular 5 deps deep" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["B"]},
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["C"]},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["D"]},
        {"buildStepName": "D", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["E"]},
        {"buildStepName": "E", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["A"]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "its only found to be circular 5 deps wide" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["B", "C", "D", "E"]},
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "D", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "E", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["A"]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "its only found to be circular 5 steps wide and 5 steps deep" do
      json = """
      [
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["B", "C", "D", "E"]},
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "D", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "E", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["F"]},
        {"buildStepName": "F", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["G"]},
        {"buildStepName": "G", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["H"]},
        {"buildStepName": "H", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["I"]},
        {"buildStepName": "I", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["A"]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "its only found to be circular 5 steps wide and 5 steps deep after many steps" do
      json = """
      [
        {"buildStepName": "B", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "C", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "D", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": []},
        {"buildStepName": "E", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["F"]},
        {"buildStepName": "F", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["G"]},
        {"buildStepName": "G", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["H"]},
        {"buildStepName": "H", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["I"]},
        {"buildStepName": "I", "commandType": "shellCommand", "command": "echo 'fuel'", "dependsOn": ["A"]},
        {"buildStepName": "A", "commandType": "shellCommand", "command": "echo 'tires'", "dependsOn": ["B", "C", "D", "E"]}
      ]
      """

      assert {:error,
              {:invalid_config,
               "I failed to parse the build_pipeline_config because I found a circular dependency: A <-> A"}} =
               ConfigFile.parse_and_validate({json, @setup})
    end

    test "x" do
      json = """
            [
        {
          "buildStepName": "find_todos",
          "commandType": "script",
          "command": "find_todos",
          "dependsOn": []
        },
        {
          "buildStepName": "deps.get",
          "commandType": "shellCommand",
          "command": "mix deps.get",
          "dependsOn": []
        },
        {
          "buildStepName": "loadconfig",
          "commandType": "shellCommand",
          "command": "mix loadconfig config/prod.exs",
          "dependsOn": []
        },
        {
          "buildStepName": "compileDev",
          "commandType": "shellCommand",
          "command": "mix compile",
          "dependsOn": [
            "deps.get"
          ],

         "envVars": [
            {
              "name": "MIX_ENV",
              "value": "dev"
            }
          ]
        },
        {
          "buildStepName": "compileTest",
          "commandType": "shellCommand",
          "command" : "mix compile --force --warnings-as-errors",
          "dependsOn": [
            "deps.get"
          ],
          "envVars": [
            {
              "name": "MIX_ENV",
              "value": "test"
            }
          ]
        },

        {
          "buildStepName": "test",
          "commandType": "shellCommand",
          "command": "mix test --color",
          "dependsOn": [
            "compileTest"
          ]
        },
        {
          "buildStepName": "escriptBuildDev",
          "commandType": "shellCommand",
          "command": "mix escript.build",
          "dependsOn": [
            "compileDev"
          ],
          "envVars": [
            {
              "name": "MIX_ENV",

             "value": "dev"
            }
          ]
        },
        {
          "buildStepName": "end_to_end_test",
          "commandType": "script",
          "command": "end_to_end_test",
          "dependsOn": [
            "compileDev"
          ]
        },
        {
          "buildStepName": "exit_code_correctness_end_to_end_test",
          "commandType": "script",
          "command": "exit_code_correctness_end_to_end_test",
          "dependsOn": [
            "compileDev"
          ]
        },
        {
          "buildStepName": "preflight_checks_return_non_zero_exit_code_test",
          "commandType": "script",
          "command": "preflight_checks_return_non_zero_exit_code_test",
          "dependsOn": [
            "compileDev"
          ]
        },
        {
          "buildStepName": "running_steps_get_terminated_properly",
          "commandType": "script",
          "command": "running_steps_get_terminated_properly",
          "dependsOn": [
            "compileDev"
          ]
        },
        {
          "buildStepName": "escriptBuild",
          "commandType": "shellCommand",
          "command": "mix escript.build",
          "dependsOn": [
            "test",
            "end_to_end_test",
            "exit_code_correctness_end_to_end_test",
            "preflight_checks_return_non_zero_exit_code_test",
            "running_steps_get_terminated_properly"
          ],
          "envVars": [
            {
              "name": "MIX_ENV",
              "value": "prod"
            }
          ]
        }
      ]
      """

      assert {:ok, _} = ConfigFile.parse_and_validate({json, @setup})
    end
  end
end
