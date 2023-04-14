import {Octokit} from 'octokit'
import {env} from 'node:process'

export const repo = {
  owner: env.GITHUB_REPOSITORY_OWNER!,
  repo: env.GITHUB_REPOSITORY!.slice(env.GITHUB_REPOSITORY_OWNER!.length + 1),
} as const

export default new Octokit({
  auth: env.GH_TOKEN,
})
