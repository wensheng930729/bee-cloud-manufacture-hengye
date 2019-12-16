import { Component } from 'react';
import { Button, Card, Table, Form, Input, Row, Col, Select, message, Divider, Modal } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getRoles,
  getList,
  change
} from './services/index';

const FormItem = Form.Item;
const Option = Select.Option;

@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    roles: [],
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    getRoles().then(res => {
      if (res.code === 1) {
        this.setState({
          roles: res.object
        })
      } else {
        message.error(res.message);
      }
    })
    this.getData({ currentPage, pageSize });
  }

  getData = ({ currentPage, pageSize }) => {
    const { validateFields } = this.props.form;
    validateFields(['keyword', 'roleId'], (err, values) => {
      if (!err) {
        let params = {
          currentPage,
          pageSize,
          keyword: values.keyword !== undefined && values.keyword !== null ? values.keyword : undefined,
          roleId: values.roleId !== undefined && values.roleId !== null ? values.roleId : undefined,
        };

        getList(params).then(res => {
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

  handleEnable = (id) => {
    let self = this;
    Modal.confirm({
      title: '禁用/启用人员',
      content: '您是否要禁用/启用人员呢？',
      onOk() {
        change(id).then(res => {
          if (res.code === 1) {
            message.success(res.message)
            self.getData({ currentPage: 1, pageSize: 10 });
          } else {
            message.error(res.message)
          }
        })
      },
      okText: '确定',
      onCancel() { },
      cancelText: '取消',
    });
  }

  render() {
    const { getFieldDecorator } = this.props.form;
    const { roles, data, currentPage, pageSize, totalPage, totalRecords } = this.state;
    const columns = [
      {
        title: 'id',
        dataIndex: 'userId',
        key: 'userId',
      }, {
        title: '姓名',
        dataIndex: 'name',
        key: 'name',
      }, {
        title: '电话',
        dataIndex: 'username',
        key: 'username',
      }, {
        title: '角色',
        dataIndex: 'roleName',
        key: 'roleName',
      }, {
        title: '创建时间',
        dataIndex: 'createTime',
        key: 'createTime',
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.handleEnable(row.userId)} style={{ color: '#1890ff', cursor: 'pointer' }}>{row.status === '已禁用' ? '启用' : '禁用'}</span>
            <Divider type="vertical" />
            <span
              onClick={() => router.push(`/permission/userManage/edit?userId=${row.userId}&name=${row.name}&username=${row.username}&roleId=${row.roleId}`)}
              style={{ color: '#1890ff', cursor: 'pointer' }}
            >编辑</span>
          </div>
        ),
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push(`/permission/userManage/edit`)} >新增</Button>} />
        <div className={styles.container}>
          <Card title="人员管理" bordered={false} >
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="关键字">
                    {
                      getFieldDecorator('keyword')(
                        <Input style={{ width: 260 }} placeholder="请输入员工姓名或电话" />
                      )
                    }
                  </FormItem>
                  <FormItem label="客户类别">
                    {
                      getFieldDecorator('roleId')(
                        <Select style={{ width: 260 }}>
                          {
                            roles.map((item, index) => <Option value={item.roleId} key={item.roleId}>{item.roleName}</Option>)
                          }
                        </Select>
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
      </div>
    )
  }
}