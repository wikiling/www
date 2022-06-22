import "./Fieldset.scss"
import { useFormContext } from "react-hook-form"
import Field from "./Field";

type FieldsetProps = {}

const Fieldset: React.FC<FieldsetProps> = ({ children }) => {
  const { formState: { errors } } = useFormContext();

  const errorValues = errors ? Object.values(errors) : [];

  return (
    <fieldset className="fieldset">
      <div className="fieldset-fields">
        {children}
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