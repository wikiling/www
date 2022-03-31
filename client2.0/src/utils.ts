
const canvas = document.createElement("canvas")
const context = canvas.getContext("2d")

export const getTextWidth = (text: string, font: string = '') => {
  if (!context) return 2
  context.font = "12pt Avenir, sans serif"
  const metrics = context.measureText(text)
  return metrics.width
}