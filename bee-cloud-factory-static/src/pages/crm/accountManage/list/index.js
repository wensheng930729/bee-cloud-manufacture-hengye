import { Component } from 'react';
import { Button, Card, Table, Form, Input, Row, Col, Select, Divider, Modal, message } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getTypeList,
  changePassword
} from '../services/index';

const FormItem = Form.Item;
const Option = Select.Option;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
    visible: false,
    modal_ID: null,
    password: null
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData = ({ currentPage, pageSize }) => {
    const { validateFields } = this.props.form;
    const { id, type } = this.props.location.query;
    validateFields(['name', 'phone'], (err, values) => {
      if (!err) {
        let params = {
          name: values.name !== undefined && values.name !== null ? values.name : undefined,
          phone: values.phone !== undefined && values.phone !== null ? values.phone : undefined,
          relatedId: id,
          type
        };

        getTypeList({ currentPage, pageSize, params }).then(res => {
          if (res && res.code === 1 && res.object) {
            this.setState({
              data: res.object,
              ...res.page
            })
          } else {
            message.error(res.message);
          }
        })
      }
    });
  }

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    this.getData({ currentPage, pageSize });
  }

  handleSearch = () => {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  handleReset = () => {
    const { resetFields } = this.props.form;
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  handlePassworad = (id) => {
    const { validateFields } = this.props.form;
    const { modal_ID } = this.state;
    validateFields(['password', 'password_two'], (err, values) => {
      if (!err) {
        let params = {
          id: modal_ID,
          password: values.password
        };
        changePassword(params).then(res => {
          if (res && res.code === 1) {
            message.success(res.message);
            this.setState({
              visible: false,
              modal_ID: null
            })
            this.getData({ currentPage: 1, pageSize: 10 });
          } else {
            message.error(res.message);
          }
        })
      }
    });
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { data, currentPage, pageSize, totalPage, totalRecords, visible, modal_ID } = this.state;
    const { id, type, name } = this.props.location.query;
    const columns = [
      {
        title: '姓名',
        dataIndex: 'name',
        key: 'name',
      }, {
        title: '注册手机号',
        dataIndex: 'phone',
        key: 'phone',
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text === 0 ? '禁用' : text === 1 ? '启用' : '',
      }, {
        title: '注册时间',
        dataIndex: 'createTime',
        key: 'createTime',
      }, {
        title: '职务',
        dataIndex: 'job',
        key: 'job',
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/crm/accountManage/edit?relatedId=${id}&type=${type}&id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <span onClick={() => this.setState({ visible: true, modal_ID: row.id })} style={{ color: '#1890ff', cursor: 'pointer' }}>修改密码</span>
          </div>
        ),
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push(`/crm/accountManage/edit?relatedId=${id}&type=${type}`)}>新增</Button>} />
        <div className={styles.container}>
          <Card title={name} bordered={false} >
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="姓名">
                    {
                      getFieldDecorator('name')(
                        <Input style={{ width: 260 }} placeholder="按联系人名称进行查询" />
                      )
                    }
                  </FormItem>
                  <FormItem label="手机号">
                    {
                      getFieldDecorator('phone')(
                        <Input style={{ width: 260 }} placeholder="按手机号进行查询" />
                      )
                    }
                  </FormItem>
                </Col>
                <Col>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
                  <Button onClick={this.handleReset} style={{ marginLeft: 24 }}>重置</Button>
                </Col>
              </Row>
            </Form>
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={{
                showQuickJumper: true,
                showSizeChanger: true,
                defaultCurrent: 1,
                defaultPageSize: 10,
                current: currentPage,
                pageSize: pageSize,
                total: totalRecords,
                onChange: this.onChange.bind(this),
                pageSizeOptions: ["10", "20", "30"],
                showTotal: (total, range) => `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>

        <Modal
          title="重置密码"
          visible={visible}
          onOk={this.handlePassworad}
          okText="保存"
          onCancel={() => { this.setState({ visible: false, modal_ID: null }) }}
        >
          <Form layout="inline">
            <FormItem label="密码">
              {
                getFieldDecorator('password', {
                  rules: [
                    { required: true, message: '请输入密码' },
                    {
                      validator: (rule, value, callback) => {
                        if (!value) {
                          callback("")
                        } else if (value.length < 6 || value.length > 16) {
                          callback("请输入6-16位的密码");
                        } else {
                          callback();
                        }
                      }
                    }
                  ]
                })(
                  <Input type="password" style={{ width: 300 }} placeholder="请输入" onChange={(e) => this.setState({ password: e.target.value })} />
                )
              }
            </FormItem>
            <FormItem style={{ marginTop: 20 }} label="再次确认密码">
              {
                getFieldDecorator('password_two', {
                  rules: [
                    { required: true, message: '请再次输入密码' },
                    {
                      validator: (rule, value, callback) => {
                        const { password } = this.state;
                        if (!value) {
                          callback("")
                        } else if (password !== value) {
                          callback("两次输入密码不相同");
                        } else {
                          callback();
                        }
                      }
                    }
                  ]
                })(
                  <Input type="password" style={{ width: 300 }} placeholder="请输入" />
                )
              }
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
  }
}