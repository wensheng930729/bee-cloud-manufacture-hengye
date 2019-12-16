import { Component } from 'react';
import { Layout, Icon, message, LocaleProvider, Breadcrumb } from 'antd';
import SiderMenu from "../components/SiderMenu/SiderMenu";
import { getMenuData } from '../common/menu';
import GlobalHeader from "../components/GlobalHeader";
import zh_CN from 'antd/lib/locale-provider/zh_CN';
import router from 'umi/router';
import { connect } from 'dva';
import { login, getSelfResource } from '../services/index'

const { Content, Header, Footer } = Layout;

@connect(({ global }) => ({
  global
}))
export default class BasicLayout extends Component {
  constructor(props) {
    super(props);
    this.state = {
      collapsed: false
    };
    loading: true
  }

  componentWillMount() {
    //80端口跳转443
    if (
      window.location.href.indexOf('http://') > -1 &&
      window.location.href.indexOf('http://1') === -1 &&
      window.location.href.indexOf('http://localhost') === -1
    ) {
      //http重定向到https
      window.location.href = window.location.href.replace('http://', 'https://')
    }
  }

  componentDidMount() {
    const { dispatch } = this.props;
    const { pathname } = this.props.location;
    if (pathname !== '/login') {
      getSelfResource().then(resp => {
        if (resp.code === 1) {
          dispatch({
            type: 'global/setMenus',
            payload: resp.object
          })
        } else {
          message.error("获取当前角色菜单失败：" + resp.message)
        }
      })
    }
  }

  handleMenuCollapse = () => {
    this.setState({
      collapsed: !this.state.collapsed,
    });
  };

  render() {
    const { children, location } = this.props;
    const { menus } = this.props.global;
    const { collapsed } = this.state;

    if (location.pathname === '/jump_page' && menus.length !== 0) {
      let jump_path = getMenuData(menus)
      router.push(jump_path[0]['children'][0]['path']);
    }

    if (location.pathname === '/login') {
      return (
        <LocaleProvider locale={zh_CN}>
          {children}
        </LocaleProvider>
      )
    } else {
      return (
        <LocaleProvider locale={zh_CN}>
          <Layout>
            <SiderMenu
              collapsed={collapsed}
              menuData={getMenuData(menus)}
              location={location}
              onCollapse={this.handleMenuCollapse}
            />
            <Layout>
              <Header style={{ padding: 0 }}>
                <GlobalHeader
                  collapsed={collapsed}
                  currentUser={{
                    name: 'Serati Ma',
                    avatar: 'https://gw.alipayobjects.com/zos/rmsportal/BiazfanxmamNRoxxVxka.png',
                    userid: '00000001',
                    notifyCount: 12,
                  }}
                  onCollapse={this.handleMenuCollapse}
                />
              </Header>
              <Content style={{ height: '100%', padding: '24px 32px' }}>
                {children}
              </Content>
            </Layout>
          </Layout>
        </LocaleProvider>
      );
    }
  }
}