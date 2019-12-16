import { Component } from 'react';
import { Form, Button, Card, Divider, Input, message, Select, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getDetail,
  saveSupplier,
  updateSupplier
} from '../services/index';

const FormItem = Form.Item;
const { Option } = Select;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    carrier: 1
  }

  componentDidMount() {
    const { id } = this.props.location.query;
    if (id !== undefined) {
      this.id = id;//供应商id
      getDetail(id).then(res => {
        if (res.code === 1) {
          const { name, telephone, address, mailbox, category, status, relatedContacts, phoneNumber, carrier, bankAccount, taxNumber, bank, billingAddress, carrierCategory } = res.object;
          this.props.form.setFieldsValue({
            name, telephone, address, mailbox, category, status,carrierCategory,
            relatedContacts, phoneNumber, carrier, bankAccount, taxNumber, bank, billingAddress
          })
          this.setState({
            carrier: res.object.carrier
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

    validateFields((err, values) => {
      if (!err) {
        let params = {};

        params = {
          ...values,
          id: this.id || undefined
        }

        if (id === undefined) {
          saveSupplier(params).then(res => {
            if (res.code === 1) {
              message.success(res.message);
              router.goBack();
            } else {
              message.error(res.message);
            }
          })
        } else {
          updateSupplier(params).then(res => {
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

  //是否为承运商监听
  carrierChange(carrier) {
    this.setState({ carrier })
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { carrier } = this.state;
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回列表</Button>} />
        <div className={styles.container}>
          <p className={styles.tableTitle}>供应商基本信息</p>
          <Form>
            <Row>
              <Col span={12}>
                <FormItem label="供应商名称">
                  {
                    getFieldDecorator('name', {
                      rules: [{ required: true, message: '请输入供应商名称' }]
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
                <FormItem label="供应商类别">
                  {
                    getFieldDecorator('category', {
                      rules: [{ required: true, message: '请选择供应商类别' }]
                    })(
                      <Select style={{ width: 320 }}>
                        <Option value={0}>核心供应商</Option>
                        <Option value={1}>战略供应商</Option>
                        <Option value={2}>储备供应商</Option>
                      </Select>
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="启用状态">
                  {
                    getFieldDecorator('status', {
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
                    })(
                      <Input style={{ width: 320 }} placeholder="请输入" />
                    )
                  }
                </FormItem>
              </Col>
              <Col span={12}>
                <FormItem label="是否为承运商">
                  {
                    getFieldDecorator('carrier', {
                      rules: [{ required: true, message: '请选择是否为承运商' }]
                    })(
                      <Select style={{ width: 320 }} onChange={this.carrierChange.bind(this)}>
                        <Option value={0}>否</Option>
                        <Option value={1}>是</Option>
                      </Select>
                    )
                  }
                </FormItem>
              </Col>
              {carrier === 1 ? <Col span={12}>
                <FormItem label="承运商类别">
                  {
                    getFieldDecorator('carrierCategory', {
                      rules: [{ required: true, message: '请选择承运商类别' }]
                    })(
                      <Select style={{ width: 320 }}>
                        <Option value={0}>公司</Option>
                        <Option value={1}>个人</Option>
                      </Select>
                    )
                  }
                </FormItem>
              </Col> : null}
            </Row>
          </Form>
          <Divider />
          <p className={styles.tableTitle}>供应商财务信息</p>
          <Form>
            <Row>
              <Col span={12}>
                <FormItem label="银行账户">
                  {
                    getFieldDecorator('bankAccount', {
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