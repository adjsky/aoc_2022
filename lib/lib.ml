let read_lines file_path =
  let ic = open_in file_path in
  In_channel.input_all ic |> String.split_on_char '\n'
