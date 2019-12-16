import { Component } from 'react';
import { Breadcrumb, Modal } from 'antd';
import withRouter from 'umi/withRouter';
import router from 'umi/router';
import styles from './index.less';

@withRouter
export default class BreadCrumb extends Component {
  handleSkip = name => {
    const { menus } = this.props;
    menus.forEach((item, index) => {
      if (item.name === name) {
        router.push(item.path);
      }
    });
  };

  render() {
    const { menus, style } = this.props;
    const { pathname } = this.props.location;
    const routerArr = pathname.split('/');
    routerArr.splice(0, 1);

    return (
      <div className={styles.container} style={{ ...style }}>
        <Breadcrumb>
          <Breadcrumb.Item>蜜云后台</Breadcrumb.Item>
          {menus.length !== 0 &&
            queryNames(menus, routerArr, "/", 1, menus.length, []).map(item => (
              <Breadcrumb.Item key={item}>
                <a onClick={this.handleSkip.bind(this, item)}>{item}</a>
              </Breadcrumb.Item>
            ))}
        </Breadcrumb>
      </div>
    );
  }
}

const queryNames = (menus, names, parentRoute, level, max, crumbs) => {
  let targetMenus = [];

  for (let i = 0; i < menus.length; i++) {
    if (menus[i].path === (parentRoute + names[0])) {
      if (menus[i].routes) {
        targetMenus = menus[i].routes;
        parentRoute = parentRoute + names[0] + '/';
      }
      crumbs.push(menus[i].name);
    }
  }

  if (level === max) {
    return crumbs;
  } else {
    names.splice(0, 1);
    return queryNames(targetMenus, names, parentRoute, level + 1, max, crumbs)
  }
};
