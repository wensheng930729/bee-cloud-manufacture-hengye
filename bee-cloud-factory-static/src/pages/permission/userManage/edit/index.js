import { Component } from 'react';
import { Form, Button, Card, Input, message, Select, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getRoles,
  save,
  update
} from '../services/index';

const FormItem = Form.Item;
const { Option } = Select;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    roles: [],
    password: null
  }

  componentDidMount() {
    getRoles().then(res => {
      if (res.code === 1) {
        this.setState({
          roles: res.object
        })
      } else {
        message.error(res.message);
      }
    })
  }

  save = () => {
    const { validateFields } = this.props.form;
    const { userId, name, username, roleId } = this.props.location.query;

    validateFields((err, values) => {
      
      if (!err) {
        let params = {};
        params = {
          userId: userId !== undefined ? userId : undefined,
          name: values.name,
          username: values.username,
          roleId: values.roleId,
          password: values.password || undefined
        }
        if (userId === undefined) {
          save(params).then(res => {
            if (res.code === 1) {
              message.success(res.message);
              router.goBack();
            } else {
              message.error(res.message);
            }
          })
        } else {
          update(params).then(res => {
            if (res.code === 1) {
              message.success(res.message);
              router.goBack();
            } else {
              message.error(res.message);
            }
          })
        }
      }
    });
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { userId, name, username, roleId } = this.props.location.query;
    const { roles } = this.state;
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回列表</Button>} />
        <div className={styles.container}>
          <Card title={userId === undefined ? "新增用户" : "编辑用户"} border={false}>
            <Form>
              <Row>
                <Col span={24}>
                  <FormItem label="客户名称">
                    {
                      getFieldDecorator('name', {
                        initialValue: userId !== undefined ? (name !== undefined ? name : null) : null,
                        rules: [{ required: true, message: '请输入客户名称' }]
                      })(
                        <Input style={{ width: 320 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="电话">
                    {
                      getFieldDecorator('username', {
                        initialValue: userId !== undefined ? (username !== undefined ? username : null) : null,
                        rules: [
                          { required: true, message: '请输入电话' },
                          {
                            validator: (rule, value, callback) => {
                              let reg = /^1(3|4|5|6|7|8|9)\d{9}$/;
                              if (!value) {
                                callback("")
                              } else if (!reg.test(value)) {
                                callback("请输入正确的11位电话");
                              } else {
                                callback();
                              }
                            }
                          }
                        ]
                      })(
                        <Input style={{ width: 320 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="角色">
                    {
                      getFieldDecorator('roleId', {
                        initialValue: userId !== undefined ? (roleId !== undefined ? Number(roleId) : null) : null,
                        rules: [{ required: true, message: '请选择角色' }]
                      })(
                        <Select style={{ width: 320 }}>
                          {
                            roles.map((item, index) => <Option value={item.roleId} key={item.roleId}>{item.roleName}</Option>)
                          }
                        </Select>
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="请输入密码">
                    {
                      getFieldDecorator('password', {
                        rules: [
                          {
                            validator: (rule, value, callback) => {
                              if (value && value.length < 6 || value.length > 16) {
                                callback("请输入6-16位的密码");
                              } else {
                                callback();
                              }
                            }
                          }
                        ]
                      })(
                        <Input style={{ width: 320 }} type="password" placeholder="请输入密码" onChange={(e) => this.setState({ password: e.target.value })} />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="请再次输入密码">
                    {
                      getFieldDecorator('password_two', {
                        rules: [
                          {
                            validator: (rule, value, callback) => {
                              const { password } = this.state;
                              if (password && !value) {
                                callback("请再次输入密码");
                              } else if (value && password !== value) {
                                callback("两次输入密码不相同");
                              } else {
                                callback();
                              }
                            }
                          }
                        ]
                      })(
                        <Input style={{ width: 320 }} type="password" placeholder="请确认密码" />
                      )
                    }
                  </FormItem>
                </Col>
              </Row>
            </Form>
            <div className={styles.btnBox}>
              <Button onClick={() => router.goBack()}>取消</Button>
              <Button onClick={this.save.bind(this)} type="primary" style={{ marginLeft: 16 }}>保存</Button>
            </div>
          </Card>
        </div >
      </div >
    )
  }
}