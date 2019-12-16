import { Component } from 'react'
import { Button, Card, Table, Divider, Form, Input, Badge, Row, Col, message, Select, Popconfirm } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../components/BreadCrumb'
import router from 'umi/router'
import { searchPlcDeviceList, deletePlcDeviceById } from '../services'
import * as utils from '@/utils/utils'

const FormItem = Form.Item
const Option = Select.Option

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

  getData({ currentPage, pageSize }) {
    const { validateFields } = this.props.form;
    validateFields((err, values) => {
      if (!err) {
        let obj = {
          currentPage,
          pageSize,
          ...values
        };
        const str = utils.queryString(obj)
        searchPlcDeviceList(str).then(res => {
          if (res && res.code === 1 && res.object) {
            this.setState({
              data: res.object,
              ...res.page
            })
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

  handleDelete = id => {
    deletePlcDeviceById(id).then(res => {
      if (res.code === 1) {
        message.success(res.message);
        this.getData({ currentPage: 1, pageSize: 10 });
      } else {
        message.error(res.message)
      }
    })
  }

  handleReset = () => {
    const { resetFields } = this.props.form
    resetFields();
    this.getData({ currentPage: 1, pageSize: 10 });
  }

  render() {
    const { getFieldDecorator } = this.props.form

    const { data, currentPage, pageSize, totalPage, totalRecords } = this.state
    const columns = [
      {
        title: '设备名称',
        dataIndex: 'name',
        key: 'name'
      }, {
        title: '品牌',
        dataIndex: 'brand',
        key: 'brand'
      }, {
        title: '型号',
        dataIndex: 'type',
        key: 'type'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text ? <span style={{ color: '#1890FF' }}>启用</span> : <span style={{ color: '#f5222d' }}>未启用</span>
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => router.push(`/device/plcManage/edit?id=${row.id}`)} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
            <Divider type="vertical" />
            <Popconfirm
              placement="topRight"
              title={'确认删除该PLC设备？'}
              onConfirm={() => this.handleDelete(row.id)}
              okText="确认"
              cancelText="取消"
            >
              <span style={{ color: '#1890ff', cursor: 'pointer' }}>删除</span>
            </Popconfirm>
          </div>
        )
      }
    ]
    return (
      <div>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.push('/device/plcManage/edit')}>新增</Button>} />
        <div className={styles.container}>
          <Card title="PLC设备管理" bordered={false}>
            <Form layout="inline">
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  <FormItem label="设备名称">
                    {getFieldDecorator('deviceName')(
                      <Input style={{ width: 260 }} placeholder="设备名称" autoComplete='off'/>
                    )}
                  </FormItem>
                  <FormItem label="状态">
                    {getFieldDecorator('status', {})(
                      <Select style={{ width: 260 }} placeholder="请选择">
                        <Option value={0}>未启用</Option>
                        <Option value={1}>启用</Option>
                      </Select>
                    )}
                  </FormItem>
                </Col>
                <Col>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
                  <Button style={{ marginLeft: 24 }} onClick={this.handleReset}>重置</Button>
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
                pageSizeOptions: ['10', '20', '30'],
                showTotal: (total, range) =>
                  `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>
      </div>
    )
  }
}
