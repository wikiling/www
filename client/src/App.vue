<script setup lang="ts">
import { reactive } from 'vue'
import { useStore } from './store'
import SyntaxTree from './components/SyntaxTree.vue'

const store = useStore()

store.dispatch('fetchAuthors');
store.dispatch('fetchTexts')

</script>

<template>
  <div v-for="author in store.state.authors" :key="author.id">
    <div v-for="text in store.getters['textsByAuthor'](author.id)" :key="text.id">
      <div class="header">
        {{ author.full_name }}, {{ text.title }}
        <hr/>
      </div>
      <div v-for="(sentence, idx) in text.sentences" :key="idx">
        <div>
          {{ `(${sentence.id})` }}
        </div>
        <SyntaxTree :sentence="sentence"/>
      </div>
    </div>
  </div>

</template>

<style lang="scss">
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: left;
  color: #2c3e50;

  .header {
    padding-bottom: 50px;
  }
}
</style>
