const clearAndUpper = (text: string) => text.replace(/-/, "").toUpperCase();

export const toCamelCase = (text: string) => text.replace(/-\w/g, clearAndUpper);
export const toPascalCase = (text: string) => text.replace(/(^\w|-\w)/g, clearAndUpper);


