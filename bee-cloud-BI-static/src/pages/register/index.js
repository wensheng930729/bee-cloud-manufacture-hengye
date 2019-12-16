import React, { Component } from 'react'
import { connect } from 'dva';
import withRouter from 'umi/withRouter';
import { Button, Row, Col, Form, Input, Radio, Icon, message, Alert } from 'antd';
import router from 'umi/router'
import Link from 'umi/link'
import styles from './index.less';
import apis from './apis'
import request from '@/utils/request';
import { regExps } from '@/utils/utils';
const FormItem = Form.Item;

@connect(({ global, loading }) => ({
  global,
  logining: loading.effects['global/login'],
}))
@Form.create()
class Login extends Component {
  constructor(props) {
    super(props)
    this.state = {
      time: 0
    }
    this.timer = null;
  }

  componentDidMount() {
    document.addEventListener("keydown", this.handleEnterKey.bind(this));
  }

  handleEnterKey = e => {
    if (e.which === 13) {
      this.handleOk()
    }
  };

  getReisterCode = () => {
    const { getFieldError, getFieldValue } = this.props.form;
    const phone = getFieldValue('phone')
    const error = getFieldError('phone')
    if (error || !phone) {
      message.info('请输入正确的11位手机号')
      return
    }
    request(apis.getReisterCode.api(phone), { method: apis.getReisterCode.type }).then(res => {
      if (res.code === 0) {
        message.success('短信发送成功,请查收')
        //计时
        this.setState({ time: 60 })
        this.timer = setInterval(() => {
          let time = this.state.time
          if (time > 1) {
            time--
            this.setState({ time })
          } else {
            clearInterval(this.timer);
            this.setState({ time: 0 })
          }
        }, 1000)
      } else {
        message.error(res.msg)
      }
    })
  }

  //注册
  handleOk() {
    const { form } = this.props;
    form.validateFieldsAndScroll((errors, values) => {
      if (errors) {
        return
      }
      if (!errors) {
        const { code, phone, password, password1 } = values;
        if (password !== password1) {
          message.error('两次输入密码不一样')
          return
        }

        //验证
        request(apis.validate.api({ phone, code, type: 0 }), { method: apis.validate.type }).then(res => {
          if (res.code === 0) {
            return true
          } else {
            message.error(res.msg)
            return false
          }
        }).then(status => {
          //注册
          if (status) {
            return request(apis.register.api(), { method: apis.register.type, body: { phone, password, name: '' } })
          } else {
            return {}
          }
        }).then(res => {
          if (res && res.code === 0) {
            message.success('注册成功！', 1.5, () => { router.replace('/login') })
          } else {
            res.msg && message.error(res.msg)
          }
        })
      }
    }
    )
  }

  componentWillUnmount() {
    //清空计时器
    if (this.timer) {
      clearInterval(this.timer)
    }

    document.removeEventListener("keydown", this.handleEnterKey);
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { global, logining } = this.props;
    const { time } = this.state;
    return (<div className={styles.app}>
      <div className={styles.panel}>
        <div className={styles.bg}></div>
        <div className={styles.wraper}>
          <img className={styles.topLeft} src={require('@/assets/login/1.png')} />
          <img className={styles.topRight} src={require('@/assets/login/2.png')} />
          <img className={styles.bottomLeft} src={require('@/assets/login/3.png')} />
          <img className={styles.bottomRight} src={require('@/assets/login/4.png')} />
          <div className={styles.left}>
            <img className={styles.logo} src={require('@/assets/login/logo.png')} />
            <div className={styles.titleC}>供应链科技管理系统</div>
            <div className={styles.titleE}>SUPPLY CHAIN MANAGEMENT SYSTEM</div>
            <div className={styles.welcome}>WELCOME</div>
          </div>
          <div className={styles.right}>
            <Form>
              <div className={styles.title}>注册</div>
              <div className={`${styles.userName} ${styles.pre}`}>
                <FormItem>
                  {getFieldDecorator('phone', {
                    rules: [
                      {
                        required: true,
                        pattern: regExps.phone,
                        message: '请输入正确的11位手机号'
                      },
                    ],
                  })(<Input maxLength={11} prefix={<span className={styles.icon}>+86</span>} placeholder='11位手机号' />)}
                </FormItem></div>
              <div className={styles.code}>
                <div className={styles.input_}>
                  <FormItem>
                    {getFieldDecorator('code', {
                      rules: [{ required: true, message: '输入验证码!' }],
                    })(<Input placeholder="输入验证码" type='number' autoComplete={false} autoCapitalize={false} />)}
                  </FormItem>
                </div><Button className={styles.button_} disabled={time > 0} onClick={this.getReisterCode}>
                  {time > 0 ? `${time}秒后失效` : `获取验证码`}</Button>
              </div>
              <div className={styles.password}>
                <FormItem>
                  {getFieldDecorator('password', {
                    rules: [
                      {
                        required: true,
                        pattern: regExps.password,
                        message: '请输入6-16位密码(数字 字母)'
                      }
                    ],
                  })(<Input type='password' placeholder='6-16位密码，区分大小写' />)}
                </FormItem></div>
              <div className={styles.password}>
                <FormItem>
                  {getFieldDecorator('password1', {
                    rules: [
                      {
                        required: true,
                        pattern: regExps.password,
                        message: '请输入6-16位密码(数字 字母)'
                      },
                    ],
                  })(<Input type='password' placeholder='确认密码' />)}
                </FormItem></div>
            </Form>

            <div className={styles.button} onClick={this.handleOk.bind(this)}>注册</div>
            <Link className={styles.toLogin} to="/login">使用已有账户登录</Link>
          </div>
        </div>
      </div>
    </div>
    )
  }
}

export default withRouter(Login)
// Login.propTypes = {
//   form: PropTypes.object,
//   dispatch: PropTypes.func,
//   loading: PropTypes.object,
// }

