import React, { useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";
import "./App.scss";
import { useStores } from "./hooks";
import { observer } from "mobx-react-lite";
import FragmentDetailRoute from "components/routes/FragmentDetailRoute";

const App: React.FC = () => {
  return (
    <div className="app">
      <BrowserRouter>
        <Routes>
          <Route path="/">
            <Route index/>
            <Route path=":fragmentSlug" element={<FragmentDetailRoute/>}/>
          </Route>
        </Routes>
      </BrowserRouter>
    </div>
  );
};

export default observer(App);
