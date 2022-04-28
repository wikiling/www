import React, { useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import "./App.scss";
import { useStores } from "./hooks";
import { observer } from "mobx-react-lite";
import AuthorTextsRoute from "components/routes/FragmentDetailRoute";

const App: React.FC = () => {
  const { fragmentStore } = useStores()

  useEffect(() => {
    fragmentStore.dispatchFetchAuthors();
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
