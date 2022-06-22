import classNames from "classnames";
import useTwoClicks from "hooks/useTwoClicks";
import { FieldValues, FormProvider, SubmitHandler, useForm, UseFormProps, UseFormReturn } from "react-hook-form";

export type FormDblClickHandler<V extends FieldValues> = (e: React.MouseEvent<HTMLFormElement, MouseEvent> , ctx: UseFormReturn<V, any>) => void

type FormProps<TFormValues> = {
  onSubmit: SubmitHandler<TFormValues>;
  children: (methods: UseFormReturn<TFormValues>) => React.ReactNode;
  options: UseFormProps<TFormValues>;
  className?: string
  onClick?: React.MouseEventHandler<HTMLFormElement>
  onDblClick?: FormDblClickHandler<TFormValues>
};

const Form = <
  TFormValues extends Record<string, any>
>({ onSubmit, children, options, className, onClick, onDblClick }: FormProps<TFormValues>) => {
  const methods = useForm<TFormValues>(options);
  const handleClick = useTwoClicks({
    onSingleClick: onClick,
    onDoubleClick: (e) => onDblClick && onDblClick(e, methods)
  });

  return (
    <FormProvider {...methods}>
      <form
        onSubmit={methods.handleSubmit(onSubmit)}
        className={classNames(className)}
        onClick={handleClick}
      >
        {children(methods)}
        <input style={{ display: "none"}} type="submit"/>
      </form>
    </FormProvider>
  );
};
 
export default Form;