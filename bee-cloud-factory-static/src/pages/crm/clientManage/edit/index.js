import { Component } from 'react';
import { Form, Button, Card, Divider, Input, message, Select, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getDetail,
  saveClient,
  updateClient
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
    const { id } = this.props.location.query;
    const { details } = this.state;

    validateFields((err, values) => {
      if (!err) {
        let params = {};

        params = {
          ...values,
          id: id !== undefined ? details.id : undefined
        }

        if (id === undefined) {
          saveClient(params).then(res => {
            if (res.code === 1) {
              message.success(res.message);
              router.goBack();
            } else {
              message.error(res.message);
            }
          })
        } else {
          updateClient(params).then(res => {
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
          <p className={styles.tableTitle}>客户基本信息</p>
          <Form>
            <Row>
              <Col span={12}>
                <FormItem label="客户名称">
                  {
                    getFieldDecorator('name', {
                      initialValue: id !== undefined ? (details.name !== undefined ? details.name : null) : null,
                      rules: [{ required: true, message: '请输入客户名称' }]
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="联系方式">
                  {
                    getFieldDecorator('telephone', {
                      initialValue: id !== undefined ? (details.telephone !== undefined ? details.telephone : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="地址">
                  {
                    getFieldDecorator('address', {
                      initialValue: id !== undefined ? (details.address !== undefined ? details.address : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>

                <FormItem label="公司邮箱">
                  {
                    getFieldDecorator('mailbox', {
                      initialValue: id !== undefined ? (details.mailbox !== undefined ? details.mailbox : null) : null,
                      rules: [{
                        validator: (rule, value, callback) => {
                          let reg = /^([A-Za-z0-9_\-\.\u4e00-\u9fa5])+\@([A-Za-z0-9_\-\.])+\.([A-Za-z]{2,8})$/;
                          if (value && !reg.test(value)) {
                            callback("邮箱格式不正确");
                          } else {
                            callback();
                          }
                        }
                      }]
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="客户类别">
                  {
                    getFieldDecorator('category', {
                      initialValue: id !== undefined ? (details.category !== undefined ? details.category : null) : null,
                      rules: [{ required: true, message: '请选择客户类别' }]
                    })(
                      <Select style={{ width: 320 }}>
                        <Option value={0}>核心客户</Option>
                        <Option value={1}>战略客户</Option>
                        <Option value={2}>储备客户</Option>
                      </Select>
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
                <FormItem label="相关联系人">
                  {
                    getFieldDecorator('relatedContacts', {
                      initialValue: id !== undefined ? (details.relatedContacts !== undefined ? details.relatedContacts : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="电话号码">
                  {
                    getFieldDecorator('phoneNumber', {
                      initialValue: id !== undefined ? (details.phoneNumber !== undefined ? details.phoneNumber : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
            </Row>
          </Form>
          <Divider />
          <p className={styles.tableTitle}>客户银行信息</p>
          <Form>
            <Row>
              <Col span={12}>
                <FormItem label="银行账户">
                  {
                    getFieldDecorator('bankAccount', {
                      initialValue: id !== undefined ? (details.bankAccount !== undefined ? details.bankAccount : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="公司税号">
                  {
                    getFieldDecorator('taxNumber', {
                      initialValue: id !== undefined ? (details.taxNumber !== undefined ? details.taxNumber : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="开户银行">
                  {
                    getFieldDecorator('bank', {
                      initialValue: id !== undefined ? (details.bank !== undefined ? details.bank : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="开票地址">
                  {
                    getFieldDecorator('billingAddress', {
                      initialValue: id !== undefined ? (details.billingAddress !== undefined ? details.billingAddress : null) : null,
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
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