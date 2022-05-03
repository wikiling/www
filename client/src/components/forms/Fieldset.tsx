import "./Fieldset.scss"
import { FieldErrors } from "react-hook-form"
import Field from "./Field";

type FieldsetProps = {
  errors?: FieldErrors
}

const Fieldset: React.FC<FieldsetProps> = ({ children, errors }) => {
  const errorValues = errors ? Object.values(errors) : [];

  return (
    <fieldset className="fieldset">
      <div className="fieldset-fields">
        {children}
        <Field type="submit"/>
      </div>
      {errors &&
        <div className="fieldset-errors">
          {errorValues.map((error, idx) =>
            <div className="fieldset-error">
              {error.message}{idx !== errorValues.length - 1 && ","}
            </div>
          )}
        </div>
      }
    </fieldset>
  );
};

export default Fieldset;