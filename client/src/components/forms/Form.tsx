import classNames from "classnames";
import { FormProvider, SubmitHandler, useForm, UseFormProps, UseFormReturn } from "react-hook-form";

type FormProps<TFormValues> = {
  onSubmit: SubmitHandler<TFormValues>;
  children: (methods: UseFormReturn<TFormValues>) => React.ReactNode;
  options: UseFormProps<TFormValues>;
  className: string
  onClick?: React.MouseEventHandler<HTMLFormElement>
};

const Form = <
  TFormValues extends Record<string, any>
>({ onSubmit, children, options, className, onClick }: FormProps<TFormValues>) => {
  const methods = useForm<TFormValues>(options);

  return (
    <FormProvider {...methods}>
      <form
        onSubmit={methods.handleSubmit(onSubmit)}
        className={classNames(className)}
        onClick={onClick}
      >
        {children(methods)}
        <input style={{ display: "none"}} type="submit"/>
      </form>
    </FormProvider>
  );
};
 
export default Form;