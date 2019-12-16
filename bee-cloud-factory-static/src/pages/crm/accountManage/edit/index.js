import { Component } from 'react';
import { Form, Button, Card, Divider, Input, message, Select, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getDetail,
  saveAccount,
  updateAccount
} from '../services/index';

const FormItem = Form.Item;
const { Option } = Select;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    details: {}
  }

  componentDidMount() {
    const { id } = this.props.location.query;
    if (id !== undefined) {
      getDetail(id).then(res => {
        if (res.code === 1) {
          this.setState({
            details: res.object
          })
        } else {
          message.error(res.message);
        }
      })
    }
  }

  save = () => {
    const { validateFields } = this.props.form;
    const { relatedId, type, id } = this.props.location.query;
    const { details } = this.state;

    validateFields((err, values) => {
      if (!err) {
        let params = {};

        params = {
          ...values,
          job: values.job || undefined,
          mailbox: values.mailbox || undefined,
          relatedId: Number(relatedId),
          type: Number(type),
          id: id !== undefined ? details.id : undefined
        }

        if (id === undefined) {
          saveAccount(params).then(res => {
            if (res.code === 1) {
              message.success(res.message);
              router.goBack();
            } else {
              message.error(res.message);
            }
          })
        } else {
          updateAccount(params).then(res => {
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
    const { id } = this.props.location.query;
    const { details } = this.state;
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回列表</Button>} />
        <div className={styles.container}>
          <p className={styles.tableTitle}>个人基本信息</p>
          <Form>
            <Row>
              <Col span={12}>
                <FormItem label="注册手机号">
                  {
                    getFieldDecorator('phone', {
                      initialValue: id !== undefined ? (details.phone !== undefined ? details.phone : null) : null,
                      rules: [{ required: true, message: '请输入' }]
                    })(
                      <Input disabled={id !== undefined ? true : false} style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="姓名">
                  {
                    getFieldDecorator('name', {
                      initialValue: id !== undefined ? (details.name !== undefined ? details.name : null) : null,
                      rules: [{ required: true, message: '请输入' }]
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="邮箱">
                  {
                    getFieldDecorator('mailbox', {
                      initialValue: id !== undefined ? (details.mailbox !== undefined ? details.mailbox : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="启用状态">
                  {
                    getFieldDecorator('status', {
                      initialValue: id !== undefined ? (details.status !== undefined ? details.status : null) : null,
                      rules: [{ required: true, message: '请选择启用状态' }]
                    })(
                      <Select style={{ width: 320 }}>
                        <Option value={1}>启用</Option>
                        <Option value={0}>禁用</Option>
                      </Select>
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="职务">
                  {
                    getFieldDecorator('job', {
                      initialValue: id !== undefined ? (details.job !== undefined ? details.job : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="是否为默认公司">
                  {
                    getFieldDecorator('defaultEnterprise', {
                      initialValue: id !== undefined ? (details.defaultEnterprise !== undefined ? details.defaultEnterprise : null) : null,
                      rules: [{ required: true, message: '请选择' }]
                    })(
                      <Select style={{ width: 320 }}>
                        <Option value={0}>否</Option>
                        <Option value={1}>是</Option>
                      </Select>
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
        </div >
      </div >
    )
  }
}