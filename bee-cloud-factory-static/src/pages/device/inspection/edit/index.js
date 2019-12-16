import { Component } from 'react';
import { Button, Card, Row, Col, Form, Input, Select } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';

const FormItem = Form.Item;
const Option = Select.Option;

@withRouter
@Form.create()
export default class Index extends Component {
  render() {
    const { getFieldDecorator } = this.props.form;
    return (
      <div>
        <BreadCrumb extra={<Button type="primary">返回</Button>} />
        <div className={styles.container}>
          <Card title="新增电表" bordered={false}>
            <Form >
              <Row>
                <Col span={12}>
                  <FormItem label="电表名称">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 300 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电表编号">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 300 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电压倍率">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 300 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电流倍率">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 300 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="电表类型">
                    {
                      getFieldDecorator('name')(
                        <Select style={{ width: 300 }} placeholder="请选择">
                          <Option value={0}>三相三线</Option>
                          <Option value={1}>三相四线</Option>
                        </Select>
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="协议类型">
                    {
                      getFieldDecorator('name')(
                        <Select style={{ width: 300 }} placeholder="请选择">
                          <Option value={0}>130协议</Option>
                          <Option value={1}>1376.1协议</Option>
                        </Select>
                      )
                    }
                  </FormItem>
                </Col>
                <Col span={12}>
                  <FormItem label="SIM卡号">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 300 }} placeholder="请输入" />
                      )
                    }
                  </FormItem>
                </Col>
              </Row>
            </Form>
          </Card>

          <div className={styles.btnBox}>
            <Button type="primary">保存</Button>
            <Button>取消</Button>
          </div>
        </div>
      </div>
    )
  }
}