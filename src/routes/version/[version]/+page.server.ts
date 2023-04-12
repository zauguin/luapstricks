import {readdir} from 'node:fs/promises'

export async function load({params: {version}}: {params: {version: string}}) {
  const entries = await readdir(`static/version/${version}/doc`, {withFileTypes: true})
  const docFiles = entries
    .filter(entry => entry.isFile() && entry.name.endsWith('.pdf'))
    .map(entry => ({
      name: entry.name.slice(0, entry.name.endsWith('-doc.pdf') ? -8 : -4),
      filename: entry.name,
    }))
  return {version, docFiles}
}
