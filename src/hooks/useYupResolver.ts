import { useCallback } from "react";
import * as yup from "yup";

type ValidationMap = {[key: string]: {
  type: string
  message: string
}}

const useYupValidationResolver = <SchemaT extends yup.AnyObjectSchema>(validationSchema: SchemaT) =>
  useCallback(async data => {
    try {
      const values = await validationSchema.validate(data, {
        abortEarly: false
      });

      return {
        values,
        errors: {}
      };
    } catch (errors: any) {

      return {
        values: {},
        errors: errors.inner.reduce(
          (memo: ValidationMap, currentError: yup.ValidationError) => currentError.path ? ({
            ...memo,
            [currentError.path]: {
              type: currentError.type ?? "validation",
              message: currentError.message
            }
          }) : memo, {}
        )
      };
    }
  }, [validationSchema]
);

const useYupResolver = (schemaObject: {}) => useYupValidationResolver(
  yup.object(schemaObject)
);

export default useYupResolver;