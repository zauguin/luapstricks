<script>
  export let data
</script>

<svelte:head>
  <title>LuaPSTricks documentation overview</title>
</svelte:head>

<main>
  <p>
    This page collects documentation of multiple PSTricks packages compiled with LuaPSTricks do demonstrate which packages work correctly and which features might still be missing.
  </p>
  
  <p>
    Please select which version of LuaPSTricks you are interested in:
  </p>
  <ul>
    {#each data.versions as version}
      <li>
        <a href={`/version/${version}`}>
          {#if version === 'trunk'}
            The current git version
          {:else}
            Version {version}
          {/if}
        </a>
      </li>
    {/each}
  </ul>
</main>

<style>
</style>
