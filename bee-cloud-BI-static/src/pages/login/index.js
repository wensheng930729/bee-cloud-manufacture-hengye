import React, { Component } from 'react'
import { connect } from 'dva'
import withRouter from 'umi/withRouter'
import { Button, Row, Form, Input, Icon, message, Alert } from 'antd'
import router from 'umi/router'
import styles from './index.less'
import { login, getSelfResource } from './services/index'
const FormItem = Form.Item

@withRouter
@Form.create()
@connect(({ global }) => ({
  global
}))
class Login extends Component {
  constructor(props) {
    super(props)
    this.state = {}
  }

  componentDidMount() { }

  handleOk = () => {
    const { form, dispatch } = this.props
    form.validateFieldsAndScroll((errors, values) => {
      if (errors) {
        return
      }
      if (!errors) {
        login({ ...values }).then(res => {
          if (res.code === 1) {
            localStorage.setItem('sysToken', res.object.sysToken)
            dispatch({
              type: 'global/getSelfInfo',
              payload: {}
            })
            // 登录成功并且拿到用户信息
            this.props.dispatch({
              type: 'global/getMenus',
              payload: {},
              callback: menus => {
                if (menus && menus.length > 0) {
                  router.push(menus[0].path);
                }
                else {
                  message.info('您还没有应用的访问权限，请联系管理员开通')
                }
              },
            });
          } else {
            message.error(res.message)
          }
        })
      }
    })
  }

  renderMessage = content => {
    return (
      <Alert style={{ marginBottom: 24 }} message={content} type="error" closable showIcon />
    )
  }

  render() {
    const { getFieldDecorator } = this.props.form
    const { global, logining } = this.props
    return (
      <div className={styles.login}>
        <div className={styles.form}>
          <div className={styles.name}>
            <span>管理系统</span>
          </div>
          <div className={styles.version}>
            {/* <span>平台版本ver{'1.00.00'}Beta</span> */}
          </div>
          {!global.login && global.message && this.renderMessage(global.message)}
          <Form>
            <FormItem hasFeedback>
              {getFieldDecorator('username', {
                rules: [{ required: true, message: '请输入您的账号' }]
              })(
                <Input prefix={<Icon type="user" style={{ color: 'rgba(0,0,0,.25)' }} />} onPressEnter={this.handleOk.bind(this)} placeholder="手机号/邮箱" />
              )}
            </FormItem>
            <FormItem hasFeedback style={{ marginBottom: 5 }}>
              {getFieldDecorator('password', {
                rules: [{ required: true, message: '请输入您的密码' }]
              })(
                <Input prefix={<Icon type="unlock" style={{ color: 'rgba(0,0,0,.25)' }} />} type="password" onPressEnter={this.handleOk.bind(this)} placeholder="登录密码" />
              )}
            </FormItem>
            <Row>
              <Button className={styles.button} type="primary" onClick={this.handleOk.bind(this)} loading={this.props.logining}>登录</Button>
            </Row>
          </Form>
          <div className={styles.footer}>
            <span>copyright 2019 金蜜工业云出品</span>
          </div>
        </div>
      </div>
    )
  }
}

export default Login