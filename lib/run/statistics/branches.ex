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
    traverse([root], root, deps)
  end

  defp traverse(acc, node, deps) do
    children = Map.fetch!(deps, node)

    case children do
      [] ->
        nest_if_not_nested_enough(acc)

      [child] ->
        append_descendants(acc, child, deps)

      _ ->
        children
        |> Enum.map(fn child -> append_descendants(acc, child, deps) end)
        |> unnest_if_too_nested()
    end
  end

  defp append_descendants(acc, child, deps) do
    child_nodes = traverse([child], child, deps)
    Enum.map(child_nodes, fn node -> acc ++ node end)
  end

  defp nest_if_not_nested_enough([node]) when is_binary(node), do: [[node]]
  defp nest_if_not_nested_enough(branches), do: branches

  defp unnest_if_too_nested(branches) do
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
end
