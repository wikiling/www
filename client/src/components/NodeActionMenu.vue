<script setup lang="ts">
defineProps<{
  onNodeRemove: () => void,
  onNodeAdd: () => void
}>()
</script>

<template>
  <div class="node-action-menu">
    <div :onClick="onNodeAdd">Add Node</div>
    <div :onclick="onNodeRemove">Remove Node</div>
  </div>
</template>

<style scoped>
  div {
    margin: 0 0.5em;
    font-weight: bold;
  }
</style>
