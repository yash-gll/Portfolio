import { Routes, Route } from "react-router-dom";
import Navbar from "./components/navbar.component";
import Editor from "./pages/editor.pages";
import UserAuthForm from "./pages/userAuthForm.page";
import { createContext, useEffect, useState } from "react";
import { lookInSession } from "./common/session";

export const UserContext = createContext({})

const App = () => {

    const [userAuth, setUserAuth] = useState({});
    useEffect(() => {
        let userInSession = lookInSession("user");
        userInSession ? setUserAuth(JSON.parse(userInSession)) : setUserAuth({access_token : null})
    }, [])

    return (
        <UserContext.Provider value={{userAuth, setUserAuth}}>
            <Routes>
                <Route path="/editor" element={<Editor/>}></Route>
                <Route path="/" element={<Navbar/>}>
                    <Route path="sign-in" element={<UserAuthForm type="sign-in"/>} />
                    <Route path="sign-up" element={<UserAuthForm type="sign-up"/>} />
                </Route>
            </Routes>
        </UserContext.Provider>
    )
}

export default App;