import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Button } from 'antd';
import router from 'umi/router';

class Exception extends Component {
  backTo = (nextPath) => {
    router.push(nextPath || '/')
  }
  render() {
    const { config, code ,mainRouter} = this.props
    return (
      <div style={{ width: '100%', minHeight: '600px', height: '100%', background: '#f5f5f5', display: 'flex', justifyContent: 'center', alignItems: 'center', }}>
        <div style={{ width: '800px', }}>
          <div style={{ width: '300px', marginRight: '60px', float: 'left' }}>
            <img src={config[code].img} />
          </div>
          <div style={{ width: '350px', height: '300px', display: 'flex', justifyContent: 'center', paddingLeft: 86, flexDirection: 'column' }}>
            <div style={{ color: '#bbb', whiteSpace: 'pre', fontSize: 50, fontWeight: 700 }}>{config[code].title}</div>
            <div style={{ color: '#bbb', fontSize: 20 }}>{config[code].des}</div>
            <Button onClick={this.backTo.bind(this,mainRouter|| config[code].nextPath)} style={{ width: '150px', height: '40px', background: '#ff8200', color: '#f8f8f8', display: 'block', fontSize: '18px', border: '0', borderRadius: '3px', marginTop: '20px' }}>{config[code].buttonText}</Button>
          </div>
        </div>
      </div>
    );
  }
}

Exception.propTypes = {

};
Exception.defaultProps = {
  code: 404,
  config: {
    403: { img: require('@/assets/notFound/403.png'), title: '403', des: '抱歉，您无权访问该页面', nextPath: '/', buttonText: '返回首页' },
    404: { img: require('@/assets/notFound/404.png'), title: '404', des: '抱歉，您访问的页面不存在', nextPath: '/', buttonText: '返回首页' },
    401: { img: 'https://gw.alipayobjects.com/zos/rmsportal/RVRUAYdCGeYNBWoKiIwB.svg', title: '无权限访问', des: '抱歉，您的账号没有绑定企业 , 请联系您的企业管理员将您添加进企业 或点击下方按钮认证企业', nextPath: '/personal/registerEnterprise', buttonText: '立即认证企业' },
  }
}

export default Exception;