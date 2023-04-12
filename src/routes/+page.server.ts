import {readdir} from 'node:fs/promises'

export async function load({params: {version}}: {params: {version: string}}) {
  const entries = await readdir(`static/version`, {withFileTypes: true})
  const versions = entries
    .filter(entry => entry.isDirectory())
    .map(entry => entry.name)
  return {versions}
}
