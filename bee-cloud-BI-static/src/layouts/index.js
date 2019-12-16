import { Component } from 'react';
import {
  Layout,
  Modal,
  message,
  Spin,
  Icon,
  LocaleProvider,
  Row,
  Col,
  Button,
  Divider,
} from 'antd';

import zhCN from 'antd/lib/locale-provider/zh_CN';
import withRouter from 'umi/withRouter';
import { connect } from 'dva';
import Media from 'react-media';
import router from 'umi/router';
import SiderMenu from '../components/SiderMenu/index';
import logo from '../assets/yay.jpg';
import GlobalHeader from '../components/GlobalHeader';
import BreadCrumb from '../components/BreadCrumb';
import { formatter } from '../utils/utils';

const { Content, Header } = Layout;
const { confirm } = Modal;

@connect(({ global }) => ({
  global,
  user: global.user,
  versionMessage: global.versionMessage,
}))
class BasicLayout extends Component {
  constructor(props) {
    super(props);
    this.state = {
      collapsed: false,
      logining: true,
      todo: [],
      visible: false,
      versionMessageList: [],
    };
  }

  componentWillMount() {
    // const a=this.props;
    // debugger
    // if(router)
  }

  componentWillUnmount = () => {
    this.setState = (state, callback) => { };
  };

  componentDidMount() {
    if (this.props.location.pathname.indexOf('/login') > -1) {
      return;
    }
    // 80端口跳转443
    if (
      window.location.href.indexOf('http://') > -1 &&
      window.location.href.indexOf('http://1') === -1 &&
      window.location.href.indexOf('http://localhost') === -1
    ) {
      // http重定向到https
      window.location.href = window.location.href.replace('http://', 'https://');
    }
    const {
      dispatch,
      global,
      location: { pathname },
    } = this.props;
    // 获取用户信息
    if (global && global.login) {
      // 已登录成功
      this.setState({
        logining: false,
      });
    } else if (localStorage.sysToken) {
      // session登录
      dispatch({
        type: 'global/getSelfInfo',
        payload: {},
        callback: (nowCompany, currentPermissionsId) => {
          // 登录成功并且拿到用户信息
          this.props.dispatch({
            type: 'global/getMenus',
            payload: {},
            callback: menus => {
              if (menus && menus.length > 0) {
                this.setState({
                  logining: false
                })
                if (pathname === '/') {
                  router.push(menus[0].pathname)
                }
              } else {
                message.info('您还没有应用的访问权限，请联系管理员开通')
              }
            },
          });
        },
      });
    } else {
      localStorage.clear();
      // message.info('登录信息验证失败，请先登录！')
      setTimeout(() => {
        router.push('/login');
      }, 1000);
    }
  }

  handleMenuCollapse = () => {
    this.setState({
      collapsed: !this.state.collapsed,
    });
  };

  // 用户信息点击事件
  onMenuClick(e, a) {
    const { dispatch } = this.props;
    switch (e.key) {
      case 'logout':
        confirm({
          title: '退出登录',
          content: '确定退出金蜜智造吗?',
          onOk() {
            dispatch({
              type: 'global/logout',
              payload: {},
            });
          },
        });
        break;
      case 'personal':
        // router.push('/personal/registerEnterprise');
        break;
      case 'update':
        if (this.state.versionMessageList.length <= 0) {
          dispatch({
            type: 'global/getVersionMessage',
            payload: {},
            callback: () => {
              let versionMessageList = this.props.global.versionMessage;
              let { visible } = this.state;
              visible = true;
              this.setState({ visible, versionMessageList });
            },
          });
        } else {
          let { visible } = this.state;
          visible = true;
          this.setState({ visible });
        }
        break;
      default:
        break;
    }
  }

  closeVersionModal = () => {
    let { visible } = this.state;
    visible = false;
    this.setState({ visible });
  };

  render() {
    const { children, location } = this.props;
    const { user, menus, versionMessage } = this.props.global;
    const { collapsed, logining } = this.state;
    if (logining) {
      return (
        <Layout style={{ paddingTop: '18rem' }}>
          <Spin indicator={<Icon type="loading" style={{ fontSize: 24 }} spin />} size="large" />
        </Layout>
      );
    }
    return (
      <LocaleProvider locale={zhCN}>
        <>
          <Layout>
            <Media query="(max-width: 599px)">
              {isMobile => (
                <SiderMenu
                  logo={logo}
                  isMobile={isMobile}
                  collapsed={collapsed}
                  menuData={formatter(menus)}
                  location={location}
                  onCollapse={this.handleMenuCollapse}
                  handleMenuSelect={this.handleMenuSelect}
                />
              )}
            </Media>
            <Layout>
              <Header style={{ padding: 0 }}>
                <GlobalHeader
                  logo={logo}
                  collapsed={collapsed}
                  currentUser={{
                    name: user.nickname,
                    avatar:
                      user.userPhoto ||
                      'https://gw.alipayobjects.com/zos/rmsportal/BiazfanxmamNRoxxVxka.png',
                    userid: user.sysToken,
                    notifyCount: 12,
                  }}
                  onMenuClick={this.onMenuClick.bind(this)}
                  onCollapse={this.handleMenuCollapse}
                />
              </Header>
              <Content
                style={{
                  height: '100%',
                  marginTop: 1,
                  // width: document.documentElement.offsetWidth - (document.documentElement.offsetWidth < 599 ? 0 : collapsed ? 80 : 256),
                  overflowX: 'hidden',
                }}
              >
                <BreadCrumb menus={menus} />
                {children}
              </Content>
            </Layout>
          </Layout>

          <Modal
            visible={this.state.visible}
            footer={null}
            closable={false}
          // onCancel={this.handleCancel}
          >
            <>
              <Row>
                <Col>
                  {this.state.versionMessageList.map((msg, index) => (
                    <div key={index}>
                      <label>版本名称：{msg.versionNum}</label>
                      <br />
                      <label>更新时间：{msg.upgradeTime}</label>
                      <br />
                      <label>更新内容：{msg.description}</label>
                      <Divider />
                    </div>
                  ))}
                </Col>
              </Row>
              <Row>
                <Col span={8} offset={10}>
                  <Button onClick={this.closeVersionModal}>退出</Button>
                </Col>
              </Row>
            </>
          </Modal>
        </>
      </LocaleProvider>
    );
  }
}

export default withRouter(BasicLayout);
