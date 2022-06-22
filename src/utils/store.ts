const { assign } = Object;

export const createIdMap = <T extends {id: any}>(objects: T[]) => {
  return objects.reduce((memo, object) => {
    return assign(memo, { [object.id]: object });
  }, {});
};