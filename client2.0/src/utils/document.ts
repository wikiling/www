
const canvas = document.createElement("canvas")
const context = canvas.getContext("2d")

export const getTextDimensions = (text: string, font: string = '') => {
  if (!context) return { width: 5, height: 5 };
  context.font = "12pt Avenir, sans serif"
  return {
    height: context.measureText('M').width,
    width: context.measureText(text).width
  }
}
