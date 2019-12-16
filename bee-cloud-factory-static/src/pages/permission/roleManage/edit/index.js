import { Component } from 'react';
import { Form, Button, Card, Input, message, Row, Col, Checkbox } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getDetail,
  save,
  getResources
} from '../services/index';

const FormItem = Form.Item;
const { TextArea } = Input;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    detail: {},
    apps: [],
    webs: [],
    bis: [],
    app_self: [],
    web_self: [],
    bis_self: []
  }

  componentDidMount() {
    const { roleId } = this.props.location.query;
    getResources().then(res => {
      if (res.code === 1) {
        this.setState({
          apps: res.object.app,
          webs: res.object.web,
          bis: res.object.bi
        })
      } else {
        message.error(res.message);
      }
    })
    if (roleId !== undefined) {
      let app_self = [], web_self = [], bis_self = [];
      getDetail(roleId).then(res => {
        if (res.code === 1) {
          res.object.app.forEach(item => {
            if (item.selection === 1) {
              app_self.push(item.resourceId)
            }
          })
          res.object.web.forEach(item => {
            if (item.selection === 1) {
              web_self.push(item.resourceId)
            }
          })
          res.object.bi.forEach(item => {
            if (item.selection === 1) {
              bis_self.push(item.resourceId)
            }
          })
          this.setState({
            detail: res.object,
            app_self,
            web_self,
            bis_self
          })
        } else {
          message.error(res.message);
        }
      })
    }
  }

  save = () => {
    const { validateFields } = this.props.form;
    const { roleId } = this.props.location.query;

    validateFields((err, values) => {
      // console.log(values)
      if (!err) {
        let params = {};
        params = {
          roleId: roleId !== undefined ? roleId : undefined,
          roleName: values.roleName,
          describe: values.describe || undefined,
          app: values.app,
          web: values.web,
          bi: values.bi
        }

        save(params).then(res => {
          if (res.code === 1) {
            message.success(res.message);
            router.goBack();
          } else {
            message.error(res.message);
          }
        })
      }
    });
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { roleId } = this.props.location.query;
    const { detail, apps, webs, bis, app_self, web_self, bis_self } = this.state;
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回列表</Button>} />
        <div className={styles.container}>
          <Card title={roleId === undefined ? "新增角色" : "编辑角色"} border={false}>
            <Form>
              <Row>
                <Col span={24}>
                  <FormItem label="角色名称">
                    {
                      getFieldDecorator('roleName', {
                        initialValue: roleId !== undefined ? (detail.roleName || null) : null,
                        rules: [{ required: true, message: '请输入角色名称' }]
                      })(
                        <Input style={{ width: 320 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="描述">
                    {
                      getFieldDecorator('describe', {
                        initialValue: roleId !== undefined ? (detail.describe || null) : null
                      })(
                        <TextArea style={{ width: 320 }} placeholder="请输入描述" autoSize={{ minRows: 6, maxRows: 8 }} />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="APP模块权限">
                    {
                      getFieldDecorator('app', {
                        initialValue: roleId !== undefined ? app_self : null
                      })(
                        <Checkbox.Group style={{ width: '100%' }}>
                          <Row>
                            {
                              apps.map((item, index) => (
                                <Col span={6} key={item.resourceId}>
                                  <Checkbox value={item.resourceId}>{item.resourceName}</Checkbox>
                                </Col>
                              ))
                            }
                          </Row>
                        </Checkbox.Group>
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="配置后台模块权限">
                    {
                      getFieldDecorator('web', {
                        initialValue: roleId !== undefined ? web_self : null
                      })(
                        <Checkbox.Group style={{ width: '100%' }}>
                          <Row>
                            {
                              webs.map((item, index) => (
                                <Col span={6} key={item.resourceId}>
                                  <Checkbox value={item.resourceId}>{item.resourceName}</Checkbox>
                                </Col>
                              ))
                            }
                          </Row>
                        </Checkbox.Group>
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={24}>
                  <FormItem label="web端模块权限">
                    {
                      getFieldDecorator('bi', {
                        initialValue: roleId !== undefined ? bis_self : null
                      })(
                        <Checkbox.Group style={{ width: '100%' }}>
                          <Row>
                            {
                              bis.map((item, index) => (
                                <Col span={6} key={item.resourceId}>
                                  <Checkbox value={item.resourceId}>{item.resourceName}</Checkbox>
                                </Col>
                              ))
                            }
                          </Row>
                        </Checkbox.Group>
                      )
                    }
                  </FormItem>
                </Col>
              </Row>
            </Form>
            <div className={styles.btnBox}>
              <Button onClick={() => router.goBack()}>取消</Button>
              <Button onClick={this.save} type="primary" style={{ marginLeft: 16 }}>保存</Button>
            </div>
          </Card>
        </div >
      </div >
    )
  }
}