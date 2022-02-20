<template>
  <div :onclick="onContainerClick" class="syntax-tree-container">
    <svg :id="treeId" width=1500 height=1000>
      <g transform="translate(500,10)">
        <g class="links"></g>
        <g class="nodes"></g>
        <g class="labels"></g>
      </g>
    </svg>
    <NodeActionMenu
      :onNodeRemove="onNodeActionMenuRemove"
      :onNodeAdd="onNodeActionMenuAdd"
      :class="{ menuIsActive: 'active' }"/>
  </div>
</template>

<script setup lang="ts">
import { reactive, onMounted } from 'vue'
import { hierarchy, HierarchyPointNode, tree } from 'd3-hierarchy'
import * as selection from 'd3-selection'
import NodeActionMenu from './NodeActionMenu.vue'
import { Sentence, SyntaxTree } from '../types'

const props = defineProps<{ sentence: Sentence }>()
const { root } = reactive({ root: hierarchy(props.sentence.syntax_tree) })
const canvas = document.createElement("canvas")
const context = canvas.getContext("2d")
const treeId = `tree-${props.sentence.id.toString()}`
const activity = reactive({ menuIsActive: false })

const getTextWidth = (text: string, font: string = '') => {
  if (!context) return 2
  context.font = "12pt Avenir, sans serif"
  const metrics = context.measureText(text)
  return metrics.width
}

const onContainerClick = (e) => {
  console.log(e)
}

const onNodeActionMenuRemove = () => {

}

const onNodeActionMenuAdd = () => {
  
}

onMounted(() => {
  var nodeWidth = 30;
  var nodeHeight = 75;
  var horizontalSeparationBetweenNodes = 16;
  var verticalSeparationBetweenNodes = 12;
  console.log(0)
  var treeLayout = tree<SyntaxTree>()
    .nodeSize([nodeWidth + horizontalSeparationBetweenNodes, nodeHeight + verticalSeparationBetweenNodes])
    .separation(function(a, b) {
      const halfWidthA = getTextWidth(a.data.text) / 2
      const halfWidthB = getTextWidth(b.data.text) / 2
      
      if (halfWidthA + halfWidthB > nodeWidth + horizontalSeparationBetweenNodes) {
        return 1.5
      }
        
      return a.parent == b.parent ? 1 : 1.25;
    });

  treeLayout(root);

  // Select the SVG element
  var svg = selection.select(`#${treeId}`);

  // Add links
  svg.select('g.links')
    .selectAll('line.link')
    .data(root.links())
    .enter()
    .append('line')
    .classed('link', true)
    .attr('x1', function(d) {return d.source.x;})
    .attr('y1', function(d) {return d.source.y;})
    .attr('x2', function(d) {return d.target.x;})
    .attr('y2', function(d) {return d.target.y;})
    .attr('stroke', "darkgray")
    .attr('stroke-width', 2);

  const width = 50
  const height = 25

  // Add nodes
  svg.select('g.nodes')
    .selectAll('rect.node')
    .data(root.descendants())
    .enter()
    .append('rect')
    .classed('node', true)
    .attr('width', width)
    .attr('height', 25)
    .attr('x', function(d) {return d.x - width / 2})
    .attr('y', function(d) {return d.y - height / 2})
    .attr("fill", "white")

  // draw labels
  svg.select('g.labels')
    .selectAll('text.label')
    .data(root.descendants())
    .enter()
    .append('text')
    .classed('label', true)
    .style('fill', 'gray')
    .attr('x', function(d) {
      const nodeWidth = getTextWidth(d.data.text)
      return d.x - nodeWidth / 2
    })
    .attr('y', function(d) {return d.y + 5;})
    .html((d) => d.data.text);
})

</script>

<style scoped lang="scss">
  .node-action-menu {
    visibility: hidden;

    &.active {
      visibility: visible;
    }
  }
</style>
