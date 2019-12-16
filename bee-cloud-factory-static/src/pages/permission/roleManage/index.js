import { Component } from 'react';
import { Button, Card, Table, Form, Input, Row, Col, Select, message, Divider, Modal } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getList,
  deleteRole
} from './services/index';

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
    totalRecords: 0
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    this.getData({ currentPage, pageSize });
  }

  getData = ({ currentPage, pageSize }) => {
    const { validateFields } = this.props.form;
    validateFields((err, values) => {
      if (!err) {
        let params = {
          currentPage,
          pageSize,
          roleName: values.roleName !== undefined && values.roleName !== null ? values.roleName : undefined,
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

  handleDelete = (id) => {
    let self = this;
    Modal.confirm({
      title: '删除角色',
      content: '您是否要删除角色呢？',
      onOk() {
        deleteRole(id).then(res => {
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
    const { data, currentPage, pageSize, totalPage, totalRecords } = this.state;
    const columns = [
      {
        title: 'id',
        dataIndex: 'roleId',
        key: 'roleId',
        width: '25%'
      }, {
        title: '角色',
        dataIndex: 'roleName',
        key: 'roleName',
        width: '25%'
      }, {
        title: '角色描述',
        dataIndex: 'describe',
        key: 'describe',
        width: '25%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => row.canEdit === 1 ? (
          <div>
            <span onClick={() => this.handleDelete(row.roleId)} style={{ color: '#1890ff', cursor: 'pointer' }}>删除</span>
            <Divider type="vertical" />
            <span onClick={() => router.push(`/permission/roleManage/edit?roleId=${row.roleId}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
          </div>
        ) : '',
        width: '25%'
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push(`/permission/roleManage/edit`)} >新增</Button>} />
        <div className={styles.container}>
          <Card title="角色管理" bordered={false} >
            <Form layout='inline'>
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="角色名称">
                    {
                      getFieldDecorator('roleName')(
                        <Input style={{ width: 260 }} placeholder="请输入角色名称" />
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