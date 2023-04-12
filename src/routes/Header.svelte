<script>
  import {NativeSelect} from '@svelteuidev/core'
  import {page} from '$app/stores'
  import github from '$lib/images/github.svg'
</script>

<header>
  <div class="corner">
    <a href="/">
      <h1>LuaPSTricks</h1>
    </a>
  </div>

  <div class="corner">
    <a href="https://github.com/zauguin/luapstricks">
      <img src={github} height="100%" alt="GitHub" />
    </a>
  </div>
</header>

<style>
  header {
    display: flex;
    justify-content: space-between;
  }

  .corner {
    height: 3em;
    padding: 1ex;
  }

  .corner h1 {
    font-size: x-large;
    margin: 0;
  }

  .corner a {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 100%;
    height: 100%;
    color: inherit;
  }

  .corner img {
    width: 3em;
    height: 3em;
    object-fit: contain;
  }
  a:hover {
    color: inherit;
  }
</style>
