import {readdir} from 'node:fs/promises'
import octokit, {repo} from '$lib/gh'

export async function load() {
  const iterator = octokit.paginate.iterator(octokit.rest.repos.listReleases, {
    ...repo,
    per_page: 100,
  })

  const release_tags = []
  for await (const { data: releases } of iterator) {
    for (const {tag_name} of releases) {
      release_tags.push(tag_name)
    }
  }

  const entries = await readdir(`static/version`, {withFileTypes: true})
  const versions = entries
    .filter(entry => entry.isDirectory())
    .map(entry => entry.name)
  return {versions: [...versions, ...release_tags]}
}
