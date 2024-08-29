/**
 * Extracts the text of a section which has special lines. The beginning line
 * starts with `;; quoter:begin:<id>`. The ending line ends with
 * `;; quoter:end:<id>`.
 *
 * TODO: generalize this further to support other languages as `;; ...` is
 *       specific to Scheme.
 *
 * TODO: think about a more performant way to do this. In case there are many
 *       sections, it's not a good idea to iterate over the whole text again and
 *       again.
 *
 * @param textRaw The text to extract from.
 * @param id The id of the section to extract.
 * @returns The extracted text.
 * */
export function extractBySection(textRaw: string, id: string): string {

  let text = "";
  let inSection = false;
  const lines = textRaw.split("\n");
  if (id === "all") {
    text = lines.filter(line => !line.startsWith(";; quoter:")).join("\n");
    return text;
  }

  for (const line of lines) {
    if (line.startsWith(`;; quoter:begin:${id}`)) {
      inSection = true;
      continue;
    }
    if (line.startsWith(`;; quoter:end:${id}`)) {
      inSection = false;
      break;
    }
    if (
      line.startsWith(";; quoter:begin") ||
      line.startsWith(";; quoter:end")
    ) {
      // avoid including other special lines in the output
      continue;
    }
    if (inSection) {
      text += `${line}\n`;
    }
  }
  return text;
}
