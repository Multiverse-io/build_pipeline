defmodule BuildPipeline.Run.Statistics.Branches do
  def branches(roots, deps) do
    branches(roots, deps, [])
  end

  defp branches([], _deps, acc) do
    acc
  end

  defp branches([root | roots], deps, acc) do
    branch = from_root(root, deps)
    acc = acc ++ branch
    branches(roots, deps, acc)
  end

  defp from_root(root, deps) do
    # traverse([root], root, deps)
    case traverse([root], root, deps) do
      [[_ | _] | _] = result -> result
      [_ | _] = result -> [result]
    end
  end

  defp traverse(acc, node, deps) do
    children = Map.fetch!(deps, node)

    case children do
      [] ->
        acc

      [child] ->
        child_nodes = traverse([child], child, deps)

        if is_nested?(child_nodes) do
          Enum.map(child_nodes, fn node -> acc ++ node end)
        else
          acc ++ child_nodes
        end

      _ ->
        Enum.map(children, fn child ->
          child_nodes = traverse([child], child, deps)

          if is_nested?(child_nodes) do
            Enum.map(child_nodes, fn node -> acc ++ node end)
          else
            acc ++ child_nodes
          end
        end)
    end
    |> unnest()
  end

  defp unnest(branches) do
    Enum.reduce(branches, [], fn branch, outer_acc ->
      if is_nested?(branch) do
        Enum.reduce(branch, outer_acc, fn inner, acc -> [inner | acc] end)
      else
        [branch | outer_acc]
      end
    end)
    |> Enum.reverse()
  end

  defp is_nested?(lists) when is_list(lists) do
    Enum.all?(lists, fn list -> is_list(list) end)
  end

  defp is_nested?(_), do: false

  # def branches(roots, deps) do
  #  roots
  #  |> Enum.flat_map(fn root -> get_branches(root, deps) end)
  #  |> flatten()
  # end

  # defp get_branches(node, deps) do
  #  nodes_deps = deps[node]

  #  rest_of_branch =
  #    if nodes_deps do
  #      Enum.flat_map(nodes_deps, fn next_node -> get_branches(next_node, deps) end)
  #    else
  #      []
  #    end

  #  [{node, rest_of_branch}]
  # end

  # defp flatten([{step, children} | _branches]) do
  #  child_count = length(children)

  #  case child_count do
  #    0 -> [step]
  #    1 -> Enum.map(flatten(children), fn tail -> [step | tail] end)
  #    _ -> Enum.map(children, fn child -> [step | flatten([child])] end)
  #  end
  # end
end
