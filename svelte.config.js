import adapter from '@sveltejs/adapter-static'
import {vitePreprocess} from '@sveltejs/kit/vite'

/** @type {import('@sveltejs/kit').Config} */
const config = {
  // Consult https://kit.svelte.dev/docs/integrations#preprocessors
  // for more information about preprocessors
  preprocess: vitePreprocess(),

  kit: {
    adapter: adapter(),
    csp: {
      directives: {
        'default-src': ['self'],
        'style-src': ['self', 'unsafe-inline'],
      },
    },
  },
}

export default config
