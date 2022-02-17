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
    {{ author.full_name }}
    <hr/>

    <div v-for="text in store.getters['textsByAuthor'](author.id)" :key="text.id">
      {{ text.title }}
      <div  v-for="(sentence, idx) in text.sentences" :key="idx">
        <SyntaxTree :sentence="sentence"/>
      </div>
    </div>
  </div>

</template>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: left;
  color: #2c3e50;
}
</style>
