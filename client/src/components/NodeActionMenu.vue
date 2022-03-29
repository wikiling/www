<script setup lang="ts">
defineProps<{
  onNodeRemove: () => void,
  onNodeAdd: () => void
}>()
</script>

<template>
  <div class="node-action-menu">
    <div class="menu-item" @click="onNodeAdd">Add Node</div>
    <div class="menu-item" @click="onNodeRemove">Remove Node</div>
  </div>
</template>

<style scoped lang="scss">
  .node-action-menu {
    border: 1px solid;

    .menu-item:not(:last-child) {
      border-bottom: 1px solid;
    }
  }

  .menu-item {
    margin: 0 0.5em;
    font-weight: bold;
    cursor: pointer;
  }
</style>
