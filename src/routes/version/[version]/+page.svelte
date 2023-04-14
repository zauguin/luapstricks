<script>
  export let data
</script>

<main>
  <h1>
    Version {data.version}
  </h1>
  <p>
    Documentation for the following packages has been compiled:
  </p>
  <ul>
    {#each data.docFiles as file}
      <li>
        <a href={file.filename}>
          {file.name}
        </a>
      </li>
    {/each}
  </ul>
</main>
