import {readdir, stat} from 'node:fs/promises'
import octokit, {repo} from '$lib/gh'

const stripSuffix = (name: string): string => name.slice(0, name.endsWith('-doc.pdf') ? -8 : -4)

export async function load({params: {version}}: {params: {version: string}}) {
  if (await (stat(`static/version/${version}/doc`).then(s => s.isDirectory()).catch(() => false))) {
    const entries = await readdir(`static/version/${version}/doc`, {withFileTypes: true})
    const docFiles = entries
      .filter(entry => entry.isFile() && entry.name.endsWith('.pdf'))
      .map(entry => ({
        name: stripSuffix(entry.name),
        filename: `/version/${version}/doc/${entry.name}`,
      }))
    return {version, docFiles}
  } else {
    const {data: {assets}} = await octokit.rest.repos.getReleaseByTag({
      ...repo,
      tag: version,
    })

    const docFiles: {name: string, filename: string}[] = assets
        .filter(({content_type}) => content_type == 'application/pdf')
        .map(({name, browser_download_url}) => ({
          name: stripSuffix(name), filename: browser_download_url
        }))

    return {version, docFiles}
  }
}
