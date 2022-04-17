import React, { useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import "./App.scss";
import { useStores } from "./hooks";
import { observer } from "mobx-react-lite";
import AuthorTextsRoute from "components/routes/AuthorTextsRoute";

const App: React.FC = () => {
  const { centralStore } = useStores()

  useEffect(() => {
    centralStore.dispatchFetchAuthors();
    centralStore.dispatchFetchTexts();
  }, []);

  return (
    <div className="app">
      <BrowserRouter>
        <Routes>
          <Route path="/">
            <Route index/>
            <Route path="authors/:authorId" element={<AuthorTextsRoute/>}/>
          </Route>
        </Routes>
      </BrowserRouter>
    </div>
  );
};

export default observer(App);
