import { message } from 'antd';
import createHistory from 'history/es/createBrowserHistory';
// user BrowserHistory
// import createHistory from 'history/createBrowserHistory';

export function config() {
  return {
    history: createHistory(),
    onError(err) {
      err.preventDefault();
      message.error(err.message);
    },
    initialState: {

    },
  };
}
